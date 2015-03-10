module Model.Kopalnia (getKopalniaRodzic) where

import Import
import Enums
import Model

--<?php if( $data[ "RodzicId" ] != null ): ?>
--    <p>
--      <span class="inobtrusive">W:</span>
--      <?=FormatujLinkBezOpisuRaw( $data[ "RodzicId" ], $pd->singleLineText( $kp->PobierzTytul( $data[ "RodzicId" ] ) ) )?><?=($data[ "Rodzic" ] != null ? ", " . $pd->singleLineText( $data[ "Rodzic" ] ) : "" )?>
--  <?php if( $data[ "DzialId" ] != null ): ?>
--      <span class="inobtrusive">(<?= $kp->PobierzRodzaj( $data[ "RodzicId" ] )?>;
--      dział: </span><?=FormatujLinkBezOpisuRaw( $data[ "DzialId" ], $pd->singleLineText( $kp->PobierzTytul( $data[ "DzialId" ] ) ) )?><span class="inobtrusive">)</span>
--  <?php else: ?>
--      <span class="inobtrusive">(<?= $kp->PobierzRodzaj( $data[ "RodzicId" ] )?>)</span>
--  <?php endif; ?>
--    </p>
--<?php elseif( $data[ "Rodzic" ] != null ): ?>
--    <p>
--      <span class="inobtrusive">W:</span>
--      <?=$pd->singleLineText( $data[ "Rodzic" ] )?>
--    </p>
--<?php endif; ?>

getKopalniaRodzic :: Kopalnia -> Text
getKopalniaRodzic kopalnia = ""