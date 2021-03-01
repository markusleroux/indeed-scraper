#!/usr/bin/env stack
-- stack --resolver lts-17.4 script 
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

import Data.String.Utils
import Text.HTML.Scalpel

import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe
import qualified Data.Set as Set
import Control.Monad.State

import GHC.Generics
import qualified Data.ByteString.Lazy as L
import Data.Aeson (encodeFile, ToJSON)
import qualified Data.Aeson.Encode.Pretty as AP


--------------------------------


data Job = Job	{ title :: String
		, url :: String
		, company :: String
		, date :: String
		, location :: String
		, qualifications :: Maybe String
		, description :: Maybe String
		} deriving (Show, Eq, Generic, ToJSON)

-- Set requires an ordering
instance Ord Job where
	job1 `compare` job2 = ( title job1 ) `compare` ( title job2 )

--------------------------------


allJobs :: String -> String -> Int -> MaybeT IO [Job]
allJobs position location page = MaybeT $ scrapeURL url jobsScraper
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
		, "&start=" ++ ( show $ page * 10 )
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

--------------------------------

data Details = Details String String


getDetails :: String -> MaybeT IO Details
getDetails url = MaybeT $ scrapeURL url detailsScraper
    where
	detailsScraper :: Scraper String Details
	detailsScraper = do
		qualsListed <- texts $ TagString "li" @: [ hasClass "jobSearch-ReqAndQualSection-item" ]
		textListed <- texts $ TagString "div" @: [ hasClass "jobsearch-jobDescriptionText" ]
		return $ Details ( concat qualsListed ) ( concat textListed )


allDetails :: String -> String -> Int -> MaybeT IO [Job]
allDetails position location page =  allJobs position location page >>= ( mapM writeDetailsMaybe )
	where
		writeDetailsMaybe :: Job -> MaybeT IO Job
		writeDetailsMaybe job = ( getDetails ( url job ) >>= ( return . writeDetails job ) ) `mplus` ( return job )

		writeDetails :: Job -> Details -> Job
		writeDetails (Job t q c d l qual desc) (Details newQual newDesc) = Job t q c d l (Just newQual) (Just newDesc)

allDetailsAllPages :: String -> String -> [MaybeT IO [Job]]
allDetailsAllPages position location = map ( allDetails position location ) [0..]

--------------------------------


hasUniqueJob :: MaybeT IO [Job] -> MaybeT IO ( Set.Set Job ) -> MaybeT IO Bool
hasUniqueJob = liftM2 $ Set.isSubsetOf . Set.fromList

hasUniqueJob' :: Maybe [Job] -> Maybe ( Set.Set Job ) -> Bool
hasUniqueJob' =  ( fromMaybe False . ) . liftM2 Set.isSubsetOf . ( liftM Set.fromList )

addToSetM :: MaybeT IO [Job] -> MaybeT IO ( Set.Set Job ) -> MaybeT IO ( Set.Set Job )
addToSetM = liftM2 $ Set.union . Set.fromList

accumulatePages :: [MaybeT IO [Job]] -> MaybeT IO ( Set.Set Job )
accumulatePages = foldM addToSetM' Set.empty
	where
	addToSetM' :: Set.Set Job -> MaybeT IO [Job] -> MaybeT IO ( Set.Set Job )
	addToSetM' jobSet = liftM $ Set.union jobSet . Set.fromList


scanlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m [a]
scanlM f q [] = return [q]
scanlM f q (x:xs) =
	do
	q2 <- f q x
	qs <- scanlM f q2 xs
	return $ (q:qs)



--------------------------------
-- https://stackoverflow.com/questions/28755554/taking-from-a-list-until-encountering-a-duplicate


remember :: [Job] -> StateT ( Set.Set Job ) ( MaybeT IO ) ()
remember = modify . Set.union . Set.fromList

hasNewJobs :: [Job] -> StateT ( Set.Set Job ) ( MaybeT IO ) Bool
hasNewJobs newJobs = do
	jobs <- get
	return $ Set.fromList newJobs `Set.isSubsetOf` jobs



remember' :: Set.Set Job -> StateT ( Set.Set Job ) ( MaybeT IO ) ()
remember' = modify . Set.union

hasNewJobs' :: Set.Set Job -> StateT ( Set.Set Job ) ( MaybeT IO ) Bool
hasNewJobs' newJobs = get >>= ( return . Set.isSubsetOf newJobs )

writeNewJobs :: [Job] -> StateT ( Set.Set Job ) ( MaybeT IO ) Bool
writeNewJobs jobs = do
	hasNew <- hasNewJobs jobs
	if hasNew then remember jobs else remember []
	return $ hasNew
	
accumulateUntil :: Monad m => ( a -> m Bool ) -> [a] -> m ()
accumulateUntil _ [] = return ()
accumulateUntil wp (h:tl) = wp h >>= evaluateB h tl
	where
	--evaluateB :: a -> [a] -> Bool -> m [a]
	evaluateB h tl b = if b then ( accumulateUntil wp tl ) >> return () else return ()

--accumulatePages' :: [MaybeT IO [Job]] -> StateT ( Set.Set Job ) ( MaybeT IO ) ()
--accumulatePages' = accumulateUntil writeNewJobs


--------------------------------
-- https://mail.haskell.org/pipermail/beginners/2009-January/000690.html


accumulatePages' :: [MaybeT IO [Job]] -> MaybeT IO ( Set.Set Job )
accumulatePages' = ( liftM stopAccum ) . scanlM addToSetM' Set.empty
	where
	addToSetM' :: Set.Set Job -> MaybeT IO [Job] -> MaybeT IO ( Set.Set Job )
	addToSetM' jobSet = liftM $ Set.union jobSet . Set.fromList

	stopAccum :: [ Set.Set a ] -> Set.Set a
	stopAccum [] = Set.empty
	stopAccum (x:[]) = x
	stopAccum (x:xs) = if length ( head xs ) == length x then x else stopAccum xs



--------------------------------


writeDetails :: String -> String -> Int -> String -> IO ()
writeDetails position location page file = do
	maybeJobs <- runMaybeT $ allDetails position location page
	case maybeJobs of
		Just jobs -> writeDetailsHelper jobs
		Nothing -> return ()
	where
		writeDetailsHelper :: [Job] -> IO ()	
		writeDetailsHelper = mapM_ $ L.appendFile file . AP.encodePretty' config

		config = AP.Config (AP.Spaces 4) mempty AP.Generic True

writeDetails' :: String -> String -> Int -> String -> IO ()
writeDetails' position location page file = ( runMaybeT $ allDetails position location page ) >>= writeMaybeList
	where
		writeMaybeList :: Maybe [Job] -> IO ()
		writeMaybeList (Just jobs) = mapM_ ( L.appendFile file . AP.encodePretty' config ) jobs
		writeMaybeList Nothing = return ()
	
		writeMaybeSet :: Maybe ( Set.Set Job ) -> IO ()
		writeMaybeSet ( Just jobs ) = mapM_ ( L.appendFile file . AP.encodePretty' config ) jobs
		writeMaybeSet Nothing = return ()
		
		config = AP.Config (AP.Spaces 4) mempty AP.Generic True


writeAllDetails :: String -> String -> Int -> String -> IO ()
writeAllDetails position location page file = ( runMaybeT $ accumulatePages $ allDetailsAllPages position location ) >>= writeMaybeSet
	where
		writeMaybeSet :: Maybe ( Set.Set Job ) -> IO ()
		writeMaybeSet ( Just jobs ) = mapM_ ( L.appendFile file . AP.encodePretty' config ) jobs
		writeMaybeSet Nothing = return ()
		
		config = AP.Config (AP.Spaces 4) mempty AP.Generic True



printDetails :: String -> String -> Int -> IO ()
printDetails position location page = ( runMaybeT $ allDetails position location page ) >>= print


--------------------------------


{-
main :: IO ()
main = encodeFile "data.json" $ allDetails "cook" "washington"
-- allDetails "cook" "washington" >>= print
-}
