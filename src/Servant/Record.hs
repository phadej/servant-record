{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Define servant servers from record types. Generics for the win.
--
-- The usage is simple, if you only need a collection of routes.
-- First you define a record with field types wrapped in a 'Functor',
-- deriving 'Generic'. Field names are irrelevant for the 'Server' part.
--
-- @
-- data Record m = Record
--     { _get :: m (Capture "id" Int :> Get '[JSON] String)
--     , _put :: m (ReqBody '[JSON] Int :> Put '[JSON] Bool)
--     }
--   deriving ('Generic')
-- @
--
-- Then you make a record of endpoint implementations. You can use
-- your monad, `Handler` or `IO`:
--
-- @
-- record :: Record (Srv IO)
-- record = Record
--    { _get = Srv $ return . show
--    _ _put = Srv $ return . odd
--    }
-- @
--
-- and finally you can turn that record into WAI 'Application' using 'serveRecord':
--
-- @
-- app :: 'Application"
-- app = 'serveRecord' liftIO record
-- @
--
module Servant.Record (
    -- * API
    recordApi,
    recordApi',
    ApiProxy (..),
    -- * Server
    serveRecord,
    Srv (..),
    recordServer,
    -- * Client
    recordClient,
    Cli (..),
    -- * Links
    --
    -- | Record approach make using safe links feature quite convinient,
    -- as we can use record accessors as proxies for endpoints.
    --
    recordLink,
    -- * Details
    -- ** Api
    HasRecordApi (..),
    HasRecordApiK1 (..),
    -- ** Server
    HasServerRecordApi (..),
    HasServerRecordApiK1 (..),
    -- ** Client
    HasClientRecordApi (..),
    HasClientRecordApiK1 (..),
    ) where

#define Type *

import GHC.Generics
import Servant
import Servant.Client.Core

-------------------------------------------------------------------------------
-- RecordApi
-------------------------------------------------------------------------------

class HasRecordApi (m :: Type -> Type) (code :: Type -> Type) where
    type RecordApi code :: Type

instance HasRecordApi m f => HasRecordApi m (M1 i c f) where
    type RecordApi (M1 i c f) = RecordApi f

instance (HasRecordApi m f, HasRecordApi m g) => HasRecordApi m (f :*: g) where
    type RecordApi (f :*: g) = RecordApi f :<|> RecordApi g

instance HasRecordApiK1 m c =>  HasRecordApi m (K1 i c) where
    type RecordApi (K1 i c) = RecordApiK1 c

class HasRecordApiK1 (m :: Type -> Type) (c :: Type) where
    type RecordApiK1 c :: Type

instance m ~ m' => HasRecordApiK1 m (f m' api) where
    type RecordApiK1 (f m' api) = api

-- | Get a proxy for an api from the record.
recordApi
    :: forall (record :: (Type -> Type) -> Type) f m code.
       ( HasRecordApi m code
       , Generic (record (f m)), Rep (record (f m)) ~ code
       )
    => record (f m)
    -> Proxy (RecordApi code)
recordApi _ = Proxy

-- | Specialized to ('ApiProxy' 'Proxy') version of 'recordApi'
--
-- @
-- api :: RecordApi (Rep (Record (ApiProxy Proxy)))
-- api = recordApi' (Proxy :: Proxy Record)
-- @
recordApi'
    :: forall (record :: (Type -> Type) -> Type) code.
       ( HasRecordApi Proxy code
       , Generic (record (ApiProxy Proxy)), Rep (record (ApiProxy Proxy)) ~ code
       )
    => Proxy record
    -> Proxy (RecordApi code)
recordApi' _ = Proxy

-- | 'Proxy' has wrong kind.
data ApiProxy (m :: * -> *) api = ApiProxy

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

-- | A newtype wrapping 'Server' implementation
newtype Srv m api = Srv { unSrv :: ServerT api m }

class HasRecordApi m code => HasServerRecordApi (m :: Type -> Type) (code :: Type -> Type) where
    recordServerCode
        :: (forall x. m x -> Handler x)
        -> code z
        -> Server (RecordApi code)

instance HasServerRecordApi m f => HasServerRecordApi m (M1 i c f) where
    recordServerCode nt (M1 rep) = recordServerCode nt rep

instance (HasServerRecordApi m f, HasServerRecordApi m g) => HasServerRecordApi m (f :*: g) where
    recordServerCode nt (f :*: g) =
        recordServerCode nt f :<|> recordServerCode nt g

instance HasServerRecordApiK1 m c =>  HasServerRecordApi m (K1 i c) where
    recordServerCode nt (K1 c) = recordServerCodeK1 nt c

class HasRecordApiK1 m c => HasServerRecordApiK1 (m :: Type -> Type) (c :: Type) where
    recordServerCodeK1
        :: (forall x. m x -> Handler x)
        -> c
        -> Server (RecordApiK1 c)

instance (HasServer api '[], m ~ m') => HasServerRecordApiK1 m (Srv m' api) where
    recordServerCodeK1 nt (Srv s) = hoistServer (Proxy :: Proxy api) nt s

-- | Get a server for an api from the record.
recordServer
    :: (HasServerRecordApi m code, Generic (record (Srv m)), Rep (record (Srv m)) ~ code)
    => (forall x. m x -> Handler x)
    -> record (Srv m)
    -> Server (RecordApi code)
recordServer nt r = recordServerCode nt (from r)

-- | Convert record into WAI 'Application'.
serveRecord
    :: ( HasServer (RecordApi code) '[]
       , HasServerRecordApi m code
       , Generic (record (Srv m)), Rep (record (Srv m)) ~ code
       )
    => (forall x. m x -> Handler x)
        -- ^ natural transformation from application monad m to 'Handler'.
        -- If your handlers are already in 'Handler', pass 'id'.
    -> record (Srv m)
        -- ^ record of endpoint implementations
    -> Application
serveRecord nt r = serve (recordApi r) (recordServer nt r)

-------------------------------------------------------------------------------
-- Client
-------------------------------------------------------------------------------

newtype Cli (m :: Type -> Type) (api :: Type) = Cli { unCli :: Client m api }

class HasRecordApi m code => HasClientRecordApi (m :: Type -> Type) (code :: Type -> Type) where
    recordClientCode
        :: Proxy m
        -> Client m (RecordApi code)
        -> code z

instance HasClientRecordApi m f => HasClientRecordApi m (M1 i c f) where
    recordClientCode m f = M1 (recordClientCode m f)

instance (HasClientRecordApi m f, HasClientRecordApi m g) => HasClientRecordApi m (f :*: g) where
    recordClientCode m (f :<|> g) = recordClientCode m f :*: recordClientCode m g

instance HasClientRecordApiK1 m c => HasClientRecordApi m (K1 i c) where
    recordClientCode m c = K1 (recordClientCodeK1 m c)

class HasRecordApiK1 m c => HasClientRecordApiK1 (m :: Type -> Type) (c :: Type) where
    recordClientCodeK1
        :: Proxy m
        -> Client m (RecordApiK1 c)
        -> c

instance m ~ m' => HasClientRecordApiK1 m (Cli m' api) where
    recordClientCodeK1 _ = Cli

-- | Generate a record of client functions.
--
-- @
-- cliRecord :: Module (Cli IO)
-- cliRecord = 'recordClient' (\x -> runClientM x env >>= either throwIO return)
--
-- cliGet :: Int -> IO String
-- cliGet = unCli (_get cliRecord)
-- @
--
recordClient
    :: ( HasClientRecordApi m code, HasClient cli (RecordApi code)
       , Generic (record (Cli m)), code ~ Rep (record (Cli m))
       )
    => (forall x. cli x -> m x)
    -> record (Cli m)
recordClient = hiddenForalls
  where
    hiddenForalls
        :: forall code m cli record. (HasClientRecordApi m code, HasClient cli (RecordApi code), Generic (record (Cli m)), code ~ Rep (record (Cli m)))
        => (forall x. cli x -> m x)
        -> record (Cli m)
    hiddenForalls nt
        = to
        $ recordClientCode m
        $ hoistClientMonad cli api nt
        $ clientIn api cli
      where
        cli = Proxy :: Proxy cli
        m   = Proxy :: Proxy m
        api = Proxy :: Proxy (RecordApi code)

-------------------------------------------------------------------------------
-- Links
-------------------------------------------------------------------------------

-- | Get a link from a record accessor.
--
-- @
-- getLink :: Int -> Link
-- getLink = 'recordLink' id _get
-- @
--
recordLink
    :: forall record code m endpoint a.
       ( HasRecordApi m code, HasLink endpoint
       , Rep (record (Srv m)) ~ code, IsElem endpoint (RecordApi code)
       )
    => (Link -> a)
    -> (record (Srv m) -> Srv m endpoint)
    -> MkLink endpoint a
recordLink toA _ =
    safeLink' toA (Proxy :: Proxy (RecordApi code)) (Proxy :: Proxy endpoint)
