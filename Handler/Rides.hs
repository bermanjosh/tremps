{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Handler.Rides where

import Import
import Data.Time.Clock (UTCTime, getCurrentTime)
import Yesod.Form.Bootstrap3


getRidesR :: Handler TypedContent
getRidesR = do
    rides <- runDB $ selectList [] [Desc RidesAdded]
    ((res, widget), enctype) <- runFormPost ridesForm 
    selectRep $ do 
        provideRep $ defaultLayout $(widgetFile "rides")
        provideRep $ return $ toJSON rides


postRidesR :: Handler ()
postRidesR = do
    ((res, entryWidget), enctype) <- runFormPost ridesForm
    case res of
        FormSuccess ride -> do
            _ <- runDB $ insert ride 
            redirect RidesR
        _ -> redirect HomeR
        
getRidesTableR :: Handler Html
getRidesTableR = do
    rides <- runDB $ selectList [] [Desc RidesAdded]
    ((res, widget), enctype) <- runFormPost ridesForm 
    return $ [shamlet|
        $forall Entity _ ride <- rides
            <tr>
                <td> #{ridesName ride}
                <td> #{ridesDest ride}
                <td> #{ridesLeaving ride}
                <td> #{ridesNumber ride}
                <td> #{ridesSpots ride}
|]


ridesForm :: Form Rides
ridesForm = renderBootstrap3 BootstrapInlineForm $ Rides
    <$> areq textField (withPlaceholder "Name" $ bfs ("Name" :: Text)) Nothing
    <*> areq textField (withPlaceholder "Destination" $ bfs ("Destination" :: Text)) Nothing
    <*> areq textField (withPlaceholder "Phone" $ bfs ("Phone" :: Text)) Nothing
    <*> areq textField (withPlaceholder "Leaving" $ bfs ("Leaving" :: Text)) Nothing
    <*> areq intField (withPlaceholder "Number of spots" $ bfs ("Number of spots" :: Text)) Nothing
    <*> lift (liftIO getCurrentTime)

testForm :: Form Test
testForm = renderBootstrap3 BootstrapInlineForm $ Test
    <$> areq textField (bfs ("test" :: Text)) Nothing