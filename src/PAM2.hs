{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PAM2 where
import Foreign
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Control.Monad
import Data.Flags.TH

cInt :: CInt
cInt = undefined

cString :: CString
cString = undefined

data PamMessage = PamMessage CInt CString
  deriving Show
instance Storable PamMessage where
  alignment _ = 8
  sizeOf _    = 16
  peek ptr    = PamMessage
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
  poke ptr (PamMessage style msg) = do
    pokeByteOff ptr 0 style
    pokeByteOff ptr 8 msg

data PamResponse = PamResponse CString CInt
instance Storable PamResponse where
  alignment _ = 8
  sizeOf    _ = 16
  peek ptr    = PamResponse
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
  poke ptr (PamResponse resp retCode) = do
    pokeByteOff ptr 0 resp
    pokeByteOff ptr 8 retCode

data PamConv = PamConv (FunPtr (CInt -> Ptr (Ptr PamMessage) -> Ptr (Ptr PamResponse) -> Ptr () -> IO CInt)) (Ptr ())
instance Storable PamConv where
  alignment _ = sizeOf nullPtr
  sizeOf    _ = 2 * sizeOf nullPtr
  peek ptr    = PamConv
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr (sizeOf nullPtr)
  poke ptr (PamConv conv appData) = do
    pokeByteOff ptr 0 conv
    pokeByteOff ptr (sizeOf nullPtr) appData

data MessageStyle = PromptEchoOff | PromptEchoOn | ErrorMsg | TextInfo
  deriving Enum
data Message = Message MessageStyle String

pamMessageFromMessage :: Message -> IO PamMessage
pamMessageFromMessage (Message style msg) = do
  msgC <- newCAString msg
  pamMessage <- return (PamMessage (fromIntegral . (+ 1) . fromEnum $ style) msgC)
  free msgC
  return pamMessage

messageFromPamMessage :: PamMessage -> IO Message
messageFromPamMessage (PamMessage style msg) = Message (toEnum . (subtract 1) . fromIntegral $ style) <$> peekCString msg

type Response = String

pamResponseFromResponse :: Response -> IO PamResponse
pamResponseFromResponse resp = do
  respC <- newCAString resp
  return (PamResponse respC 0)

data Conv a = Conv a (a -> [Message] -> IO [Response])

foreign import ccall "wrapper" createConvFunPtr :: (CInt -> Ptr (Ptr PamMessage) -> Ptr (Ptr PamResponse) -> Ptr () -> IO CInt) -> IO (FunPtr (CInt -> Ptr (Ptr PamMessage) -> Ptr (Ptr PamResponse) -> Ptr () -> IO CInt))

pamConvFromConv :: Storable a => Conv a -> IO PamConv
pamConvFromConv (Conv appData conv) = PamConv <$> convC <*> appDataC
  where convC :: IO (FunPtr (CInt -> Ptr (Ptr PamMessage) -> Ptr (Ptr PamResponse) -> Ptr () -> IO CInt))
        convC = createConvFunPtr (\numMsgC msgC respC appDataC -> do
          let numMsg :: Int
              numMsg = fromIntegral numMsgC
          msgP <- peekArray numMsg msgC
          msg <- mapM ((messageFromPamMessage =<<) . peek) msgP
          resp <- mallocArray numMsg
          appData <- peek . castPtr $ appDataC
          pokeArray resp =<< mapM pamResponseFromResponse =<< conv appData msg
          poke respC resp
          return 0
          )
        appDataC = do
          ptr <- malloc
          poke ptr appData
          return . castPtr $ ptr

freePamConv :: PamConv -> IO ()
freePamConv (PamConv conv appData) = do
  freeHaskellFunPtr conv
  free appData

data PamHandleT = PamHandleT
type PamSession = Ptr PamHandleT

data PamErr = OpenErr | SymbolErr | ServiceErr | SystemErr | BufErr | PermDenied | AuthErr | CredInsufficient | AuthInfoUnavail | UserUnknown | MaxTries | NewAuthTokReqd | AcctExpired | SessionErr | CredUnavail | CredExpired | CredErr | NoModuleData | ConvErr | AuthTokErr | AuthTokRecoveryErr | AuthTokLockBusy | AuthTokDisableAging | TryAgain | Ignore | Abort | AuthTokExpired | ModuleUnknown | BadItem | ConvAgain | Incomplete
  deriving (Enum, Show)
toErr :: CInt -> PamErr
toErr = toEnum . (subtract 1) . fromIntegral

foreign import ccall pam_strerror :: PamSession -> CInt -> IO CString
errToString :: PamErr -> IO String
errToString = (peekCString =<<) . pam_strerror nullPtr . fromIntegral . (+ 1) . fromEnum

data PamReturnM a = Success a | Err PamErr
data PamReturnT m a = PamReturnT (m (PamReturnM a))
peekPamReturnT :: PamReturnT m a -> m (PamReturnM a)
peekPamReturnT (PamReturnT m) = m
type PamReturn = PamReturnT IO PamSession

foldPamReturn :: (a -> b) -> (PamErr -> b) -> PamReturnM a -> b
foldPamReturn successH _    (Success x) = successH x
foldPamReturn _        errH (Err err  ) = errH err

instance Functor (PamReturnM) where
  fmap f (Success x) = Success . f $ x
  fmap f (Err err  ) = Err err

instance Applicative (PamReturnM) where
  pure  = Success
  (<*>) = ap

instance Monad (PamReturnM) where
  (Success x) >>= f = f x
  (Err err  ) >>= _ = Err err
  (Success _) >>  x = x
  (Err err  ) >>  _ = Err err
  fail _            = Err Abort

instance Monad m => Functor (PamReturnT m) where
  fmap f (PamReturnT m) = PamReturnT . fmap (fmap f) $ m

instance Monad m => Applicative (PamReturnT m) where
  pure  = PamReturnT . pure . pure
  (<*>) = ap

instance Monad m => Monad (PamReturnT m) where
  (PamReturnT m)  >>= f              = PamReturnT $ do
    pamRet <- m
    case pamRet of
      Success x -> peekPamReturnT $ f x
      Err err   -> return $ Err err

  (PamReturnT m1) >> (PamReturnT m2) = PamReturnT (m1 >> m2)
  fail = PamReturnT . fail

toReturn :: PamSession -> CInt -> PamReturnM PamSession
toReturn session 0 = Success session
toReturn _       e = Err . toErr $ e 

silent :: CInt
silent = 0x8000

foreign import ccall pam_start :: CString -> CString -> Ptr PamConv -> Ptr PamSession -> IO CInt
start :: String -> String -> PamConv -> PamReturn
start serviceName user pamConv = PamReturnT $ do
  serviceNameC <- newCAString serviceName
  userC <- newCAString user
  pamConvP <- malloc
  poke pamConvP pamConv
  sessionP <- malloc
  retVal <- pam_start serviceNameC userC pamConvP sessionP
  free serviceNameC
  free userC
  session <- peek sessionP
  free sessionP
  return (toReturn session retVal)

pamFWrap :: (PamSession -> CInt -> IO CInt) -> CInt -> PamSession -> PamReturn
pamFWrap f flags session = PamReturnT (toReturn session <$> f session flags)

foreign import ccall pam_end :: PamSession -> CInt -> IO CInt
end = pamFWrap pam_end

foreign import ccall pam_authenticate :: PamSession -> CInt -> IO CInt
type AuthFlags = CInt
disallowNullAuthTok :: AuthFlags
disallowNullAuthTok = 0x0001
authenticate :: AuthFlags -> PamSession -> PamReturn
authenticate = pamFWrap pam_authenticate

foreign import ccall pam_setcred :: PamSession -> CredFlags -> IO CInt
type CredFlags = CInt
establish :: CredFlags
establish = 0x0002
delete :: CredFlags
delete = 0x0004
reinitialize :: CredFlags
reinitialize = 0x0008
refresh :: CredFlags
refresh = 0x0010
setCred :: CredFlags -> PamSession -> PamReturn
setCred = pamFWrap pam_setcred

foreign import ccall pam_acct_mgmt :: PamSession -> AuthFlags -> IO CInt
acctMgmt :: AuthFlags -> PamSession -> PamReturn
acctMgmt = pamFWrap pam_acct_mgmt

foreign import ccall pam_chauthtok :: PamSession -> ChAuthTokFlags -> IO CInt
type ChAuthTokFlags = CInt
changeExpiredAuthTok :: ChAuthTokFlags
changeExpiredAuthTok = 0x0020
chAuthTok :: ChAuthTokFlags -> PamSession -> PamReturn
chAuthTok = pamFWrap pam_chauthtok

foreign import ccall pam_open_session :: PamSession -> CInt -> IO CInt
openSession = pamFWrap pam_open_session

foreign import ccall pam_close_session :: PamSession -> CInt -> IO CInt
closeSession = pamFWrap pam_close_session

foreign import ccall pam_putenv :: PamSession -> CString -> IO CInt
putEnv :: String -> PamSession -> PamReturn
putEnv nameValue session = PamReturnT $ do
  nameValueC <- newCAString nameValue
  retVal <- toReturn session <$> pam_putenv session nameValueC
  free nameValueC
  return retVal

foreign import ccall pam_getenv :: PamSession -> CString -> IO CString
getEnv :: String -> PamSession -> IO String
getEnv name session = do
  nameC <- newCAString name
  let value = peekCString =<< pam_getenv session nameC
  free nameC
  value


