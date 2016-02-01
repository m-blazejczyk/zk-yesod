module Handler.KopalniaWidgets (
    getRodzicW,
    getWydawcaW,
    getAutorW,
    getBiblioFooterW
    ) where

import Import
import Enums (showRodzaj)
import Handler.Autor (autorzyToFieldValue)

-- The approach used in the functions below is not DRY at all, but at the same time it simplifies 
-- the conditional HTML building a lot.

-- Format:
--   <span class="inobtrusive">W:</span> RodzicId.Tytul, RodzicOpis <span class="inobtrusive">(RodzicId.Rodzaj; dział:</span> DzialId->Tytul <span class="inobtrusive">)</span>
-- Options (RodzicId can be a Kopalnia or a NkPub; NkPubs don't have Dzial): 
--   W: RodzicId, RodzicOpis (RodzajRodzica; dział: DzialId)
--   W: RodzicId, RodzicOpis (RodzajRodzica)
--   W: RodzicId (RodzajRodzica; dział: DzialId)
--   W: RodzicId (RodzajRodzica)
--   W: RodzicOpis
-- Parameters:
--   getRodzicW rodzicOpis rodzic dzial nkRodzic
getRodzicW :: Maybe Text -> Maybe Kopalnia -> Maybe Kopalnia -> Maybe NkPub -> Widget
getRodzicW (Just opis) (Just rodzic) (Just dzial) Nothing =
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        <a href=@{KopalniaItemR (kopalniaLookupId rodzic)} class="link1">#{kopalniaTytul rodzic}#
        \, #{opis}
        <span class="inobtrusive">(#{showRodzaj $ kopalniaRodzaj rodzic}; dział:
        <a href=@{KopalniaItemR (kopalniaLookupId dzial)} class="link1">#{kopalniaTytul dzial}
        <span class="inobtrusive">)
    |]
getRodzicW Nothing     (Just rodzic) (Just dzial) Nothing = 
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        <a href=@{KopalniaItemR (kopalniaLookupId rodzic)} class="link1">#{kopalniaTytul rodzic}
        <span class="inobtrusive">(#{showRodzaj $ kopalniaRodzaj rodzic}; dział:
        <a href=@{KopalniaItemR (kopalniaLookupId dzial)} class="link1">#{kopalniaTytul dzial}
        <span class="inobtrusive">)
    |]
getRodzicW (Just opis) (Just rodzic) Nothing      Nothing = 
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        <a href=@{KopalniaItemR (kopalniaLookupId rodzic)} class="link1">#{kopalniaTytul rodzic}#
        \, #{opis}
        <span class="inobtrusive">(#{showRodzaj $ kopalniaRodzaj rodzic})
    |]
getRodzicW Nothing     (Just rodzic) Nothing      Nothing = 
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        <a href=@{KopalniaItemR (kopalniaLookupId rodzic)} class="link1">#{kopalniaTytul rodzic}
        <span class="inobtrusive">(#{showRodzaj $ kopalniaRodzaj rodzic})
    |]
getRodzicW (Just opis) Nothing       Nothing      (Just nkRodzic) = 
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        <a href="#" class="link1">#{nkPubTytul nkRodzic}#
        \, #{opis}
        <span class="inobtrusive">(#{showRodzaj $ nkPubRodzaj nkRodzic})
    |]
getRodzicW Nothing     Nothing       Nothing      (Just nkRodzic) = 
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        <a href="#" class="link1">#{nkPubTytul nkRodzic}
        <span class="inobtrusive">(#{showRodzaj $ nkPubRodzaj nkRodzic})
    |]
getRodzicW (Just opis) Nothing       Nothing      Nothing = 
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        #{opis}
    |]
getRodzicW Nothing     Nothing       Nothing      Nothing = 
    return ()
getRodzicW _ _ _ _ = 
    return ()  -- This is an error condition

getOnlyWydawcyW :: [Wydawca] -> Widget
getOnlyWydawcyW wydawcy =
    toWidget [hamlet|
    $forall wydawca <- wydawcy
        <a href="#" class="link1">#{wydawcaNazwa wydawca}</a>
    |]

-- Format:
--   <b>Wydawcy</b>, MiejsceWyd PubRok
-- Opcje:
--   Wydawcy, Miejsce Rok
--   Wydawcy, Miejsce
--   Wydawcy Rok
--   Wydawcy
--   Miejsce Rok
--   Miejsce
getWydawcaW :: [Wydawca] -> Maybe Text -> Maybe Int64 -> Widget
getWydawcaW [] (Just miejsce) (Just rok) = 
    toWidget [hamlet|
        #{miejsce} #{show rok}
    |]
getWydawcaW wydawcy (Just miejsce) (Just rok) = 
    toWidget [whamlet|
        <b>^{getOnlyWydawcyW wydawcy}#
        \, #{miejsce} #{show rok}
    |]
getWydawcaW [] (Just miejsce) Nothing = 
    toWidget [hamlet|
        #{miejsce}
    |]
getWydawcaW wydawcy (Just miejsce) Nothing = 
    toWidget [whamlet|
        <b>^{getOnlyWydawcyW wydawcy}#
        \, #{miejsce}
    |]
getWydawcaW wydawcy Nothing (Just rok) = 
    toWidget [whamlet|
        <b>^{getOnlyWydawcyW wydawcy}
        #{show rok}
    |]
getWydawcaW wydawcy Nothing Nothing = 
    toWidget [whamlet|
        <b>^{getOnlyWydawcyW wydawcy}
    |]

getBiblioFooterW :: Widget
getBiblioFooterW = toWidget [hamlet|
    <div class="biblio-footer">
      <div class="row">
        <div class="col-md-2 right">
          <a href="http://www.mkidn.gov.pl/" target="_blank"><img src=@{StaticR img_mkidn_biblio_png} width="118" height="70">
        <div class="col-md-2">
          <strong>Dofinansowano ze środków Ministerstwa Kultury i Dziedzictwa Narodowego.
        <div class="col-md-2 right">
          <a href="http://lib.amu.edu.pl/" target="_blank"><img src=@{StaticR img_bu_logo_biblio_png} width="98" height="70">
        <div class="col-md-2">
          Projekt wspierany przez Bibliotekę Uniwersytecką w Poznaniu.
        <div class="col-md-2 right">
          <a href="http://fundacja-ikp.pl/" target="_blank"><img src=@{StaticR img_ikp_logo_biblio_png} width="147" height="63">
        <div class="col-md-2">
          Projekt realizowany we współpracy z Fundacją Instytut Kultury Popularnej.
    |]

getAutorW :: Bool -> Int64 -> [Autor] -> Text -> Text -> Text -> Text -> Widget
getAutorW isEdit lookupId autorzy param style label popupLabel = toWidget [hamlet|
    $if isEdit
      <p>
        <span class="inobtrusive">#{label}
        <span style="#{style}">
          <span id="#{param}" data-type="select2" data-value="#{autorzyToFieldValue autorzy}" data-pk="#{lookupId}" data-url=@{KopalniaItemUpdateR} data-title="#{popupLabel}">
    $else
      $if length autorzy > 0
        <p>
          <span class="inobtrusive">#{label}
          $forall autor <- autorzy
            <span style="#{style}">
              <a href=@{AutorR (autorLookupId autor)} class="link1">
                $maybe imiona <- autorImiona autor
                  #{imiona} #
                #{autorNazwisko autor} #
    |]
