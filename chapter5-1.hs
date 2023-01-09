module Main where

import Data.List

type Attribute = (Name, AttValue)
type AttValue = Either Value [Reference]
type Name = String
type Value = String
type Reference = String

-- Sample list of simple attributes:
simple_attrs = [ ( "xml:lang", Left "en" )
               , ( "xmlns", Left "jabber:client" )
               , ( "xmlns:stream", Left "http://etherx.jabber.org/streams" ) ]

-- Sample list of attributes with references:
complex_attrs = [ ( "xml:lang", Right ["lang"] )
                , ( "lang", Left "en" )
                , ( "xmlns", Right ["ns","subns"] )
                , ( "ns", Left "jabber" )
                , ( "subns", Left "client" )
                , ( "xmlns:stream", Left "http://etherx.jabber.org/streams" ) ]

lookupAttr :: Name -> [Attribute] -> Maybe Value
lookupAttr nm attrs = 
  -- First, we lookup 'Maybe AttValue' by name and
  -- check whether we are successfull:
  case (lookup nm attrs) of
       -- Pass the lookup error through.
       Nothing   -> Nothing 
       -- If given name exist, see if it is value of reference:
       Just attv -> case attv of
                         -- It's a value. Return it!
                         Left val -> Just val
                         -- It's a list of references :(
                         -- We have to look them up, accounting for
                         -- possible failures.
                         -- First, we will perform lookup of all references ...
                         Right refs -> let vals = [ lookupAttr ref attrs | ref <- refs ]
                                           -- .. then, we will exclude lookup failures
                                           wo_failures = filter (/=Nothing) vals
                                           -- ... find a way to remove annoying 'Just' wrapper
                                           stripJust (Just v) = v
                                           -- ... use it to extract all lookup results as strings
                                           strings = map stripJust wo_failures
                                           in
                                           -- ... finally, combine them into single String. 
                                           -- If all lookups failed, we should pass failure to caller.
                                           case null strings of
                                             True  -> Nothing
                                             False -> Just (concat (intersperse ":" strings))
