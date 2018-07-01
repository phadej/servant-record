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
    -- * Server
    serveRecord,
    Srv (..),
    recordServer,
    recordApi,
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
-- Server
-------------------------------------------------------------------------------

-- | A newtype wrapping 'Server' implementation
newtype Srv m api = Srv { unSrv :: ServerT api m }

class HasServerRecordApi (m :: Type -> Type) (code :: Type -> Type) where
    type ServerRecordApi code :: Type

    recordServerCode
        :: (forall x. m x -> Handler x)
        -> code z
        -> Server (ServerRecordApi code)

instance HasServerRecordApi m f => HasServerRecordApi m (M1 i c f) where
    type ServerRecordApi (M1 i c f) = ServerRecordApi f

    recordServerCode nt (M1 rep) = recordServerCode nt rep

instance (HasServerRecordApi m f, HasServerRecordApi m g) => HasServerRecordApi m (f :*: g) where
    type ServerRecordApi (f :*: g) = ServerRecordApi f :<|> ServerRecordApi g

    recordServerCode nt (f :*: g) =
        recordServerCode nt f :<|> recordServerCode nt g

instance HasServerRecordApiK1 m c =>  HasServerRecordApi m (K1 i c) where
    type ServerRecordApi (K1 i c) = ServerRecordApiK1 c

    recordServerCode nt (K1 c) = recordServerCodeK1 nt c

class HasServerRecordApiK1 (m :: Type -> Type) (c :: Type) where
    type ServerRecordApiK1 c :: Type

    recordServerCodeK1
        :: (forall x. m x -> Handler x)
        -> c
        -> Server (ServerRecordApiK1 c)

instance (HasServer api '[], m ~ m') => HasServerRecordApiK1 m (Srv m' api) where
    type ServerRecordApiK1 (Srv m' api) = api

    recordServerCodeK1 nt (Srv s) = hoistServer (Proxy :: Proxy api) nt s

-- | Get a proxy for an api from the record.
recordApi
    :: (HasServerRecordApi m code, Generic (record (Srv m)), Rep (record (Srv m)) ~ code)
    => record (Srv m)
    -> Proxy (ServerRecordApi code)
recordApi _ = Proxy

-- | Get a server for an api from the record.
recordServer
    :: (HasServerRecordApi m code, Generic (record (Srv m)), Rep (record (Srv m)) ~ code)
    => (forall x. m x -> Handler x)
    -> record (Srv m)
    -> Server (ServerRecordApi code)
recordServer nt r = recordServerCode nt (from r)

-- | Convert record into WAI 'Application'.
serveRecord
    :: ( HasServer (ServerRecordApi code) '[]
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

class HasClientRecordApi (m :: Type -> Type) (code :: Type -> Type) where
    type ClientRecordApi code :: Type

    recordClientCode
        :: Proxy m
        -> Client m (ClientRecordApi code)
        -> code z

instance HasClientRecordApi m f => HasClientRecordApi m (M1 i c f) where
    type ClientRecordApi (M1 i c f) = ClientRecordApi f

    recordClientCode m f = M1 (recordClientCode m f)

instance (HasClientRecordApi m f, HasClientRecordApi m g) => HasClientRecordApi m (f :*: g) where
    type ClientRecordApi (f :*: g) = ClientRecordApi f :<|> ClientRecordApi g

    recordClientCode m (f :<|> g) = recordClientCode m f :*: recordClientCode m g

instance HasClientRecordApiK1 m c => HasClientRecordApi m (K1 i c) where
    type ClientRecordApi (K1 i c) = ClientRecordApiK1 c

    recordClientCode m c = K1 (recordClientCodeK1 m c)

class HasClientRecordApiK1 (m :: Type -> Type) (c :: Type) where
    type ClientRecordApiK1 c :: Type

    recordClientCodeK1
        :: Proxy m
        -> Client m (ClientRecordApiK1 c)
        -> c

instance m ~ m' => HasClientRecordApiK1 m (Cli m' api) where
    type ClientRecordApiK1 (Cli m' api) = api

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
    :: ( HasClientRecordApi m code, HasClient cli (ClientRecordApi code)
       , Generic (record (Cli m)), code ~ Rep (record (Cli m))
       )
    => (forall x. cli x -> m x)
    -> record (Cli m)
recordClient = hiddenForalls
  where
    hiddenForalls
        :: forall code m cli record. (HasClientRecordApi m code, HasClient cli (ClientRecordApi code), Generic (record (Cli m)), code ~ Rep (record (Cli m)))
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
        api = Proxy :: Proxy (ClientRecordApi code)

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
       ( HasServerRecordApi m code, HasLink endpoint
       , Rep (record (Srv m)) ~ code, IsElem endpoint (ServerRecordApi code)
       )
    => (Link -> a)
    -> (record (Srv m) -> Srv m endpoint)
    -> MkLink endpoint a
recordLink toA _ =
    safeLink' toA (Proxy :: Proxy (ServerRecordApi code)) (Proxy :: Proxy endpoint)
