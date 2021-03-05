#!/usr/bin/env stack
-- stack --resolver lts-17.4 script
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.String.Utils
import Text.HTML.Scalpel hiding (position)

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Control.Monad.State
import Data.Maybe
import qualified Data.Set as Set

import Data.Aeson (ToJSON, encodeFile)
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as L
import GHC.Generics

import System.Console.CmdArgs

--------------------------------
main :: IO ()
main = writeAllDetails =<< cmdArgs options

data ExperienceLevel
    = Entry
    | Mid
    | Senior
    deriving (Data, Eq, Typeable)

instance Show ExperienceLevel where
    show Entry = "entr y_level"
    show Mid = "mid_   level"
    show Senior = "senior_level"

data Options =
    Options
        { positionArg :: String
        , locationArg :: String
        , fileArg :: String
        , max_pagesArg :: Int
        , experienceArg :: ExperienceLevel
        }
    deriving (Data, Eq, Show, Typeable)

options :: Options
options =
    Options
        { positionArg = def &= argPos 0 &= typ "TITLE"
        , locationArg = def &= argPos 1 &= typ "LOCATION"
        , fileArg = "job_results.json"
              &= typFile &= opt ("results.json" :: String)
              &= help "The file to write to (default=results.json)"
              &= explicit
              &= name "file"
              &= name "f"
        , max_pagesArg = 50
              &= typ "INT"
              &= help "The maximum number of pages to search (default=50)"
              &= opt (50 :: Int)
              &= explicit
              &= name "max_pages"
              &= name "m"
        , experienceArg = Entry
              &= typ ""
              &= help "One of [Entry, Mid, Senior] (default=Entry)"
              &= opt Entry
              &= explicit
              &= name "exp"
              &= name "e"
        }
        &= summary "Indeed Scraper, v1.0, Markus Le Roux 2021"
        &= program "indeed-scraper"

--------------------------------
data Job =
    Job
        { title :: String
        , url :: String
        , company :: String
        , date :: String
        , location :: String
        , qualifications :: Maybe String
        , description :: Maybe String
        }
    deriving (Eq, Generic, Show, ToJSON)

-- Set requires an ordering
instance Ord Job where
    job1 `compare` job2 = (title job1) `compare` (title job2)

--------------------------------
-- Get a list of the basic information from a job search page
allJobs :: Options -> Int -> MaybeT IO [Job]
allJobs options page = MaybeT $ scrapeURL url jobsScraper
  where
    url :: String
    url = let encode = replace " " "%20" in
        concat
            [ "https://www.indeed.com/jobs?"
            , "q=" ++ (encode $ positionArg options)
            , "&l=" ++ (encode $ locationArg options)
            , "&jt=fulltime"
            , "&explvl=" ++ (show $ experienceArg options)
            , "&sort=date"
            , "&start=" ++ (show $ page * 10)
            ]
       
    jobsScraper :: Scraper String [Job]
    jobsScraper = chroots (TagString "div" @: [hasClass "result"]) jobScraper

    jobScraper :: Scraper String Job
    jobScraper = do
        titleListed <- attr "title" $ TagString "a" @: [hasClass "jobtitle"]
        urlListed <- fmap ("https://www.indeed.com" ++) $ attr "href" $ TagString "a" @: [hasClass "jobtitle"]
        companyListed <- fmap (replace "\n" "") $ text $ TagString "a" @: [AttributeString "data-tn-element" @= "companyName"]
        dateListed <- text $ TagString "span" @: [hasClass "date"]
        locationListed <- text $ TagString "span" @: [hasClass "location"]
        return $ Job titleListed urlListed companyListed dateListed locationListed Nothing Nothing

--------------------------------
data Details =
    Details String String

-- Get details from the details page for a specific job
getDetails :: String -> MaybeT IO Details
getDetails url = MaybeT $ scrapeURL url detailsScraper
  where
    detailsScraper :: Scraper String Details
    detailsScraper = do
        qualsListed <- texts $ TagString "li" @: [hasClass "jobSearch-ReqAndQualSection-item"]
        textListed <- texts $ TagString "div" @: [hasClass "jobsearch-jobDescriptionText"]
        return $ Details (concat qualsListed) (concat textListed)

-- Extend all jobs function to include details
allDetails :: Options -> Int -> MaybeT IO [Job]
allDetails options page = allJobs options page >>= (mapM writeDetailsMaybe)
  where
    writeDetailsMaybe :: Job -> MaybeT IO Job
    writeDetailsMaybe job = (getDetails (url job) >>= (return . writeDetails job)) `mplus` (return job)
    
    writeDetails :: Job -> Details -> Job
    writeDetails (Job t q c d l qual desc) (Details newQual newDesc) = Job t q c d l (Just newQual) (Just newDesc)

-- Iterate over all pages in search
allDetailsAllPages :: Options -> [MaybeT IO [Job]]
allDetailsAllPages options = map (allDetails options) [0 .. p]
  where
    p = max_pagesArg options

--------------------------------
-- https://mail.haskell.org/pipermail/beginners/2009-January/000690.html
-- Accumulate all pages into a set
accumulatePages :: [MaybeT IO [Job]] -> MaybeT IO (Set.Set Job)
accumulatePages = (liftM stopAccum) . scanlM addToSetM' Set.empty
  where
    addToSetM' :: Set.Set Job -> MaybeT IO [Job] -> MaybeT IO (Set.Set Job)
    addToSetM' jobSet = liftM $ Set.union jobSet . Set.fromList
    
        -- Indeed does not list the final page
        -- Stop when new pages don't add new jobs
    stopAccum :: [Set.Set a] -> Set.Set a
    stopAccum [] = Set.empty
    stopAccum (x:[]) = x
    stopAccum (x:xs) =
        if length (head xs) == length x
            then x
            else stopAccum xs
                 
        -- Scanl generalized to monads
    scanlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m [a]
    scanlM f q [] = return [q]
    scanlM f q (x:xs) = do
        q2 <- f q x
        qs <- scanlM f q2 xs
        return $ (q : qs)

--------------------------------
-- Write details from a specific page to file
writeDetailsPage :: Options -> Int -> IO ()
writeDetailsPage options page = (runMaybeT $ allDetails options page) >>= writeMaybeList
  where
    writeMaybeList :: Maybe [Job] -> IO ()
    writeMaybeList (Just jobs) = mapM_ (L.appendFile (fileArg options) . AP.encodePretty' config) jobs
    writeMaybeList Nothing = return ()
    
    config = AP.Config (AP.Spaces 4) mempty AP.Generic True

printDetails :: Options -> Int -> IO ()
printDetails options page = (runMaybeT $ allDetails options page) >>= print

-- Write details from all pages to file
writeAllDetails :: Options -> IO ()
writeAllDetails options = do
    jobSet <- runMaybeT $ accumulatePages $ allDetailsAllPages options
    writeMaybeSet jobSet
  where
    writeMaybeSet :: Maybe (Set.Set Job) -> IO ()
    writeMaybeSet (Just jobs) = mapM_ (L.appendFile (fileArg options) . AP.encodePretty' config) jobs
    writeMaybeSet Nothing = return ()
    
    config = AP.Config (AP.Spaces 4) mempty AP.Generic True
--------------------------------
