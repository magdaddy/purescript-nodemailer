module NodeMailer
  ( AuthConfig
  , TransportConfig
  , Message
  , MessageOptional
  , MkMessage
  , Transporter
  , MessageInfo
  , createTransporter
  , createTestAccount
  , getTestMessageUrl
  , sendMail
  , sendMail_
  ) where

import Prelude

import ConvertableOptions (class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Foreign (Foreign, unsafeToForeign)
import NodeMailer.Attachment (Attachment)
import Simple.JSON (write)

type AuthConfig =
  { user :: String
  , pass :: String
  }

type TransportConfig r =
  { host :: String
  , port :: Int
  , secure :: Boolean
  , auth :: AuthConfig
  | r
  }

type TestAccount =
  { user :: String
  , pass :: String
  , smtp :: { host :: String, port :: Int, secure :: Boolean }
  }

type Message =
  { from :: String
  , to :: Array String
  , subject :: String
  , text :: String
  | MessageOptional
  }

type MessageOptional =
  ( cc :: Array String
  , bcc :: Array String
  , html :: Maybe String
  , attachments :: Array Attachment
  )

defaultOptions :: Record MessageOptional
defaultOptions =
  { cc: []
  , bcc: []
  , html: Nothing
  , attachments: []
  }

data MkMessage = MkMessage

foreign import data Transporter :: Type

foreign import data MessageInfo :: Type

createTransporter :: forall r. TransportConfig r -> Effect Transporter
createTransporter config = runEffectFn1 createTransporterImpl config

mkMessage :: forall r. 
  ConvertOptionsWithDefaults MkMessage (Record MessageOptional) (Record r) Message =>
  Record r -> Message
mkMessage msg = convertOptionsWithDefaults MkMessage defaultOptions msg

sendMail :: Message -> Transporter -> Aff Unit
sendMail message transporter = void $ sendMail_ message transporter

sendMail_ :: Message -> Transporter -> Aff MessageInfo
sendMail_ message transporter = fromEffectFnAff $ runFn2 sendMailImpl (write message) transporter

unsafeSendMail :: forall r. Record r -> Transporter -> Aff MessageInfo
unsafeSendMail message transporter = fromEffectFnAff $ runFn2 sendMailImpl (unsafeToForeign message) transporter

createTestAccount :: Aff (TransportConfig ())
createTestAccount = do
  account <- fromEffectFnAff createTestAccountImpl
  pure
    { host: account.smtp.host
    , port: account.smtp.port
    , secure: account.smtp.secure
    , auth: { user: account.user, pass: account.pass }
    }

getTestMessageUrl :: MessageInfo -> Maybe String
getTestMessageUrl = runFn3 getTestMessageUrlImpl Nothing Just

foreign import createTransporterImpl :: forall r. EffectFn1 (TransportConfig r) Transporter

foreign import sendMailImpl :: Fn2 Foreign Transporter (EffectFnAff MessageInfo)

foreign import createTestAccountImpl :: EffectFnAff TestAccount

foreign import getTestMessageUrlImpl
  :: Fn3 (Maybe String) (String -> Maybe String) MessageInfo (Maybe String)
