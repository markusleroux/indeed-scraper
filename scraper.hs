#!/usr/bin/env stack
-- stack --resolver lts-17.4 script 
{-# LANGUAGE OverloadedStrings #-}

import Data.String.Utils
import Text.HTML.Scalpel
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.IO.Class

data Job = Job	{ title :: String
		, url :: String
		, company :: String
		, date :: String
		, location :: String
		, qualifications :: Maybe String
		, description :: Maybe String
		} deriving (Show)

allJobs :: String -> String -> MaybeT IO [Job]
allJobs position location = MaybeT $ scrapeURL url jobsScraper
    where 
	url :: String
	url = buildURL position location

	buildURL :: String -> String -> String
	buildURL position location = concat
		[ "https://www.indeed.com/jobs?"
		, "q=" ++ (encode position)
		, "&l=" ++ (encode location)
		, "&jt=fulltime"
		, "&explvl=entry_level"
		, "&sort=date"
		] where encode = replace " " "%20"

	jobsScraper :: Scraper String [Job]
	jobsScraper = chroots ( TagString "div" @: [ hasClass "result" ] ) jobScraper

	jobScraper :: Scraper String Job
	jobScraper = do
		titleListed <- attr "title" $ TagString "a" @: [ hasClass "jobtitle" ]
		urlListed <- fmap ( "https://www.indeed.com" ++ ) $ attr "href" $ TagString "a" @: [ hasClass "jobtitle" ]
		companyListed <- fmap (replace "\n" "") $ text $ TagString "a" @: [ AttributeString "data-tn-element" @= "companyName" ]
		dateListed <- text $ TagString "span" @: [ hasClass "date" ]
		locationListed <- text $ TagString "span" @: [ hasClass "location" ]

		return $ Job titleListed urlListed companyListed dateListed locationListed Nothing Nothing


data Details = Details String String


getDetails :: String -> MaybeT IO Details
getDetails url = MaybeT $ scrapeURL url detailsScraper
    where
	detailsScraper :: Scraper String Details
	detailsScraper = do
		qualsListed <- texts $ TagString "li" @: [ hasClass "jobSearch-ReqAndQualSection-item" ]
		textListed <- texts $ TagString "div" @: [ hasClass "jobsearch-jobDescriptionText" ]
		return $ Details ( concat qualsListed ) ( concat textListed )


-- Version One
------------------------------

addDetails2 :: MaybeT IO Job -> MaybeT IO Details -> MaybeT IO Job
addDetails2 = runMaybeTFunction2 $ liftM2 tryAddDetails
	where
		tryAddDetails :: Maybe Job -> Maybe Details -> Maybe Job
		tryAddDetails maybeJob maybeDetails = liftM2 writeDetails maybeJob maybeDetails `mplus` maybeJob

		writeDetails :: Job -> Details -> Job
		writeDetails (Job t q c d l qual desc) (Details newQual newDesc) = Job t q c d l (Just newQual) (Just newDesc)
	
		runMaybeTFunction2 :: (m0 (Maybe a) -> m1 (Maybe b) -> m2 (Maybe c)) -> MaybeT m0 a -> MaybeT m1 b -> MaybeT m2 c
		runMaybeTFunction2 f x y = MaybeT $ f ( runMaybeT x ) ( runMaybeT y )


addDetails :: MaybeT IO Job -> MaybeT IO Job
addDetails maybeTJob = do
	job <- maybeTJob
	addDetails2 maybeTJob $ getDetails ( url job )


-- Version Two
------------------------------

addDetails2' :: Job -> MaybeT IO Details -> MaybeT IO Job
addDetails2' job = runMaybeTFunction $ tryAddDetails' job
	where
		tryAddDetails' :: Job -> IO (Maybe Details) -> IO (Maybe Job)
		tryAddDetails' job = liftM $ return . writeDetailsMaybe job

		writeDetailsMaybe :: Job -> Maybe Details -> Job
		writeDetailsMaybe job maybeDetails = case maybeDetails of
							Just details -> writeDetails job details
							Nothing -> job

		writeDetails :: Job -> Details -> Job
		writeDetails (Job t q c d l qual desc) (Details newQual newDesc) = Job t q c d l (Just newQual) (Just newDesc)

		runMaybeTFunction :: (m0 (Maybe a) -> m1 (Maybe b)) -> MaybeT m0 a -> MaybeT m1 b
		runMaybeTFunction f x = MaybeT $ f ( runMaybeT x )

addDetails' :: Job -> MaybeT IO Job
addDetails' job = addDetails2' job $ getDetails ( url job )

allDetails :: String -> String -> IO (Maybe [Job])
allDetails position location = runMaybeT $ allJobs position location >>= ( mapM addDetails' )


-- Version Three
------------------------------

allDetails' :: String -> String -> IO (Maybe [Job])
allDetails' position location = runMaybeT $ allJobs position location >>= ( mapM writeDetailsMaybe )
	where
		writeDetailsMaybe :: Job -> MaybeT IO Job
		writeDetailsMaybe job = ( getDetails ( url job ) >>= ( return . writeDetails job ) ) `mplus` ( return job )

		writeDetails :: Job -> Details -> Job
		writeDetails (Job t q c d l qual desc) (Details newQual newDesc) = Job t q c d l (Just newQual) (Just newDesc)


------------------------------


{-
main :: IO ()
main = allDetails "cook" "washington" >>= print
-}
