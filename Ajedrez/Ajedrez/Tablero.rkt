#lang racket
;Se invoca la libreria a utilizar.
(require graphics/graphics)
( define ( Abrir-pantalla-de-juego )
;----------------------------------------------STRINGS----------------------------------------------------------------------
( define fichas ( make-string 64 #\u0080 ) )
( define ( Convertir inicio final )
   ( if ( < inicio final )
        ( begin
           ( string-set! fichas inicio ( integer->char inicio ) )
           ( Convertir ( + inicio 1 ) final )
           );Final del begin
        (void)
        );Final del if
   );Final de la funcion Convertir.
( Convertir 0 64 )
( define Posicion ( string-copy "T2C2A2R2K2A2C2T2N2N2N2N2N2N2N2N2----------------------------------------------------------------B1B1B1B1B1B1B1B1T1C1A1R1K1A1C1T1") )
( define Imagenes "
    P1.pngT2.pngC2.pngA2.pngR2.pngK2.pngA2.pngC2.pngT2.pngN2.pngN2.pngN2.pngN2.pngN2.pngN2.pngN2.pngN2.pngB1.pngB1.pngB1.pngB1.pngB1.pngB1.pngB1.pngB1.pngT1.pngC1.pngA1.pngR1.pngK1.pngA1.pngC1.pngT1.png" )
( define x2 ( string-copy " " ) )
( define y2 ( string-copy " " ) )
( define x3 ( string-copy " " ) )
( define y3 ( string-copy " " ) )
( define add_viejo ( string-copy " " ) )
( define add_nuevo ( string-copy " " ) )
( define turno ( string-copy "1" ) )
( define add-rey-blanco ( string-copy " " ) )
( string-set! add-rey-blanco 0 ( integer->char 120 ) )
( define add-rey-negro ( string-copy " " ) )
( string-set! add-rey-negro 0 ( integer->char 8 ) )
( define remplazado ( string-copy "  " ) )

( define ( Cambiar_turno )
   ( if ( equal? turno "1" )
        ( string-set! turno 0 #\2 )
        ( string-set! turno 0 #\1 )
        )
   )
( define ( Valor_Absoluto n )
   ( if ( < n 0 )
        ( * n -1 )
        n )
   );Final de la funcion Valor_Absoluto.
;-------------------------------------------------TABLERO-------------------------------------------------------------------------
( open-graphics )
( define Pantalla_De_Juego (open-viewport "Pantalla_De_Juego1" 1025  680 ) )
;----------------------------------------------HACER-TABLERO----------------------------------------------------------------------
(((draw-pixmap-posn "Madera_Fondo.png") Pantalla_De_Juego) (make-posn -1 0))
((draw-solid-rectangle Pantalla_De_Juego ) (make-posn 25 25 ) 650 650 "Brown" )
((draw-solid-rectangle Pantalla_De_Juego ) (make-posn 40 40 ) 620 620 "SaddleBrown" )
((draw-rectangle Pantalla_De_Juego ) (make-posn 40 40 ) 620 620 )
((draw-rectangle Pantalla_De_Juego ) (make-posn 25 25 ) 650 650 )
;Se establece la fórmula recursiva para imprimir el tablero
( define ( Tablero Contadorx Contadory x y )
   ( cond
      [ ( < y 600 ) 
        ( if  [ and { <= Contadorx 4 } { <= Contadory 4 } ]
              ( begin
                 ((draw-solid-rectangle Pantalla_De_Juego ) (make-posn x y ) 75 75 "MediumForestGreen" )
                 ((draw-solid-rectangle Pantalla_De_Juego ) (make-posn ( + x 75 ) y ) 75 75 "Wheat" )
                 ((draw-solid-rectangle Pantalla_De_Juego ) (make-posn ( + 75 x) ( + 75 y ) ) 75 75 "MediumForestGreen" )
                 ((draw-solid-rectangle Pantalla_De_Juego ) (make-posn x ( + 75 y ) ) 75 75 "Wheat" )
                 ( Tablero ( + Contadorx 1 ) Contadory ( + x 150 ) y )
                 );Final begin
              ( Tablero ( - Contadorx 4 ) ( + Contadory 1 ) ( - x 600 ) ( + y 150 ) )
              )];Final del if 
      [ else (void) ]
      );Final del cond
   );Final de la funcion del tablero
( Tablero 1 1 50 50 );Se invoca ek tablero.
;----------------------------------------------CLICKS-----------------------------------------------------------------------------
( define ( Hacer_click1 )
   
   ( if ( equal? #t (Jaque-mate 0 1 1 0 1 1 ) )
       ( ( ( draw-pixmap-posn "jaque-mate.png" ) Pantalla_De_Juego ) ( make-posn 190 300 ) )
        (void))
   ( mostrar-jaque )
   ( define Click1 ( get-mouse-click Pantalla_De_Juego ) )
   ( if [and (>= (posn-x (mouse-click-posn Click1)) 725 )
             (>=(posn-y (mouse-click-posn Click1)) 570 )
             (<=(posn-x (mouse-click-posn Click1)) 801 )
             (<=(posn-y (mouse-click-posn Click1)) 646 )]
        ( begin  (close-viewport Pantalla_De_Juego) )
        (void)
        );Final del if
   ( if [and (>= (posn-x (mouse-click-posn Click1)) 50 )
             (>=(posn-y (mouse-click-posn Click1)) 50 )
             (<=(posn-x (mouse-click-posn Click1)) 650 )
             (<=(posn-y (mouse-click-posn Click1)) 650 )]
        ( begin  ( Hallar_casilla1 Click1 0 0 -2 ) )
        ( Hacer_click1 )
        );Final del if
   );Final de la funcio hacer click
( define ( Hacer_click2 )
   ( ( ( draw-pixmap-posn "invalido1.png" ) Pantalla_De_Juego ) ( make-posn 715 350 ) )
   ( define Click2 ( get-mouse-click Pantalla_De_Juego ) )
   ( if [and (>= (posn-x (mouse-click-posn Click2)) 725 )
             (>=(posn-y (mouse-click-posn Click2)) 570 )
             (<=(posn-x (mouse-click-posn Click2)) 801 )
             (<=(posn-y (mouse-click-posn Click2)) 646 )]
        ( begin  (close-viewport Pantalla_De_Juego) )
        (void)
        );Final del if
   ( if [and (>= (posn-x (mouse-click-posn Click2)) 50 )
             (>=(posn-y (mouse-click-posn Click2)) 50 )
             (<=(posn-x (mouse-click-posn Click2)) 650 )
             (<=(posn-y (mouse-click-posn Click2)) 650 )]
        ( begin  ( Hallar_casilla2 Click2 0 0 -2 ) )
        ( Hacer_click1 )
        );Final del if
   );Final de la funcio hacer click
( define ( Hallar_casilla1 click contx conty add )
   ( cond
      [ (and 
         (>= (posn-x (mouse-click-posn click)) 50 )
         (>=(posn-y (mouse-click-posn click)) 50 )
         (<=(posn-x (mouse-click-posn click)) ( + 50 ( * 75 contx ) ) )
         (<=(posn-y (mouse-click-posn click)) ( + 50 ( * 75 conty ) ) ) )
        ( string-set! x2 0 ( integer->char contx ) )
        ( string-set! y2 0 ( integer->char conty ) )
        ( string-set! add_viejo 0 ( integer->char add ) )
        ( Verificar ( - add 16 ) contx conty ) ]
      [ ( <= contx 7 ) ( Hallar_casilla1 click ( + 1 contx ) conty  ( + 2 add ) ) ]
      [ ( >= contx 7 ) ( Hallar_casilla1 click  ( - contx 7 ) ( + 1 conty ) ( + 2 add) ) ]
      [ else ( Hacer_click1 ) ]
      )
   );Final de la funcion Hallar_casilla

( define ( Hallar_casilla2 click contx conty add )
   ( cond
      [ (and 
         (>= (posn-x (mouse-click-posn click)) 50 )
         (>=(posn-y (mouse-click-posn click)) 50 )
         (<=(posn-x (mouse-click-posn click)) ( + 50 ( * 75 contx ) ) )
         (<=(posn-y (mouse-click-posn click)) ( + 50 ( * 75 conty ) ) ) )
        ( string-set! x3 0 ( integer->char contx ) )
        ( string-set! y3 0 ( integer->char conty ) )
        ( string-set! add_nuevo 0 ( integer->char add ) )
        ]
      [ ( <= contx 7 ) ( Hallar_casilla2 click ( + 1 contx ) conty  ( + 2 add ) ) ]
      [ ( >= contx 7 ) ( Hallar_casilla2 click  ( - contx 7 ) ( + 1 conty ) ( + 2 add) ) ]
      [ else ( Hacer_click1 ) ]
      )
   );Final de la funcion Hallar_casilla
;-----------------------------------------------NO-COMER-EL-REY----------------------------------------------------------------
( define ( Come_el_rey? )
   ( if  ( equal? ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 16 ) ) #\K )
         #t
         #f
         );Final del if
   )
;----------------------------------------------NO-SALTAR-FICHAS----------------------------------------------------------------
( define ( Evitar_Salto x y x1 y1 add )
   ( cond
      ;Para moverse en diagonal
      [ ( and ( > x x1 ) ( > y y1 ) ( equal? #\- ( string-ref Posicion ( - add 18 ) ) ) ) ( Evitar_Salto ( - x 1 ) ( - y 1 ) x1 y1 ( - add 18 ) ) ]
      [ ( and ( > x x1 ) ( < y y1 ) ( equal? #\- ( string-ref Posicion ( + add 14 ) ) ) ) ( Evitar_Salto ( - x 1 ) ( + y 1 ) x1 y1 ( + add 18 ) ) ]
      [ ( and ( < x x1 ) ( > y y1 ) ( equal? #\- ( string-ref Posicion ( - add 14 ) ) ) ) ( Evitar_Salto ( + x 1 ) ( - y 1 ) x1 y1 ( - add 14 ) ) ]
      [ ( and ( < x x1 ) ( < y y1 ) ( equal? #\- ( string-ref Posicion ( + add 18 ) ) ) ) ( Evitar_Salto ( + x 1 ) ( + y 1 ) x1 y1 ( + add 18 ) ) ]
      ;Para moverse en linea recta
      [ ( and ( > x x1 ) ( = y y1 ) ( equal? #\- ( string-ref Posicion ( - add 2 ) ) ) ) ( Evitar_Salto ( - x 1 ) y x1 y1 ( - add 2 ) ) ]
      [ ( and ( < x x1 ) ( = y y1 ) ( equal? #\- ( string-ref Posicion ( + add 2 ) ) ) ) ( Evitar_Salto ( + x 1 ) y x1 y1 ( + add 2 ) ) ]
      [ ( and ( = x x1 ) ( > y y1 ) ( equal? #\- ( string-ref Posicion ( - add 16 ) ) ) ) ( Evitar_Salto x ( - y 1 ) x1 y1 ( - add 16 ) ) ]
      [ ( and ( = x x1 ) ( < y y1 ) ( equal? #\- ( string-ref Posicion ( + add 16 ) ) ) ) ( Evitar_Salto x ( + y 1 ) x1 y1 ( + add 16 ) ) ]
      [ ( and ( = ( + 1 x ) x1 ) ( = y y1 ) ) #t ]
      [ ( and ( = ( - x 1 ) x1 ) ( = y y1 ) ) #t ]
      [ ( and ( = x x1 ) ( = ( + 1 y ) y1 ) ) #t ]
      [ ( and ( = x x1 ) ( = ( - y 1 ) y1 ) ) #t ]
      [ ( and ( = ( + 1 x ) x1 ) ( = ( + 1 y ) y1 ) ) #t ]
      [ ( and ( = ( - x 1 ) x1 ) ( = ( + 1 y ) y1 ) ) #t ]
      [ ( and ( = ( - x 1 ) x1 ) ( = ( - y 1 ) y1 ) ) #t ]
      [ ( and ( = ( + 1 x ) x1 ) ( = ( - y 1 ) y1 ) ) #t ]
      [ ( and ( = x x1 ) ( = y y1 ) ) #t ]
      [ else #f ]
      );Final del cond
   )
;----------------------------------------------POSICIONES-DE-LAS-FICHAS--------------------------------------------------------
( define ( misma_posicion_rey? )
   ( if ( equal? turno "1" )
        (string-set! add-rey-blanco 0 ( integer->char ( - ( char->integer ( string-ref add_nuevo 0 )) 16 )) )
        (string-set! add-rey-negro 0 ( integer->char ( - ( char->integer ( string-ref add_nuevo 0 )) 16 )) )
        )
   ( if { not ( and ( = ( char->integer ( string-ref x2 0 )) ( char->integer ( string-ref x3 0 )) ) ( = ( char->integer ( string-ref y2 0 )) ( char->integer ( string-ref y3 0 )) )) }
        ( if ( and ( equal? #f ( Revisar_casillas-blanco )) ( equal? #f ( Revisar_casillas-negro )) )
             ( Cambiar_posicion_rey ( char->integer ( string-ref x3 0 )) ( char->integer ( string-ref y3 0 )) ( string-append ( string ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 16 ) )) ( string ( string-ref Posicion ( - ( + 1 ( char->integer ( string-ref add_viejo 0 )) ) 16 ) )) ".png" ) ( - ( char->integer ( string-ref add_viejo 0 ) ) 16) ( - ( char->integer ( string-ref add_nuevo 0 )) 16 ) )
             ( Cambiar_posicion_salir_jaque ( char->integer ( string-ref x3 0 )) ( char->integer ( string-ref y3 0 )) ( string-append ( string ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 16 ) )) ( string ( string-ref Posicion ( - ( + 1 ( char->integer ( string-ref add_viejo 0 )) ) 16 ) )) ".png" ) ( - ( char->integer ( string-ref add_viejo 0 ) ) 16) ( - ( char->integer ( string-ref add_nuevo 0 )) 16 ) )
             )
        ( Hacer_click1 ))
   ( Hacer_click1 )
   );FInal de la funcion misma_posicion_rey?
( define ( misma_posicion? )
   ( if { not ( and ( = ( char->integer ( string-ref x2 0 )) ( char->integer ( string-ref x3 0 )) ) ( = ( char->integer ( string-ref y2 0 )) ( char->integer ( string-ref y3 0 )) )) }
        ( if ( and ( equal? #f ( Revisar_casillas-blanco )) ( equal? #f ( Revisar_casillas-negro )) )
             ( Cambiar_posicion ( char->integer ( string-ref x3 0 )) ( char->integer ( string-ref y3 0 )) ( string-append ( string ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 16 ) )) ( string ( string-ref Posicion ( - ( + 1 ( char->integer ( string-ref add_viejo 0 )) ) 16 ) )) ".png" ) ( - ( char->integer ( string-ref add_viejo 0 ) ) 16) ( - ( char->integer ( string-ref add_nuevo 0 )) 16 ) )
             ( Cambiar_posicion_salir_jaque ( char->integer ( string-ref x3 0 )) ( char->integer ( string-ref y3 0 )) ( string-append ( string ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 16 ) )) ( string ( string-ref Posicion ( - ( + 1 ( char->integer ( string-ref add_viejo 0 )) ) 16 ) )) ".png" ) ( - ( char->integer ( string-ref add_viejo 0 ) ) 16) ( - ( char->integer ( string-ref add_nuevo 0 )) 16 ) )
             )
        ( begin ( ( ( draw-pixmap-posn "invalido.png" ) Pantalla_De_Juego ) ( make-posn 715 350 ) ) ( Hacer_click1 )))
   ( Hacer_click1 )
   );FInal de la funcion misma_posicion?
( define ( Verificar add x y )
   ( cond
      [ ( equal? ( string-ref Posicion add ) #\T ) ( Hacer_click2 )
                                                   ( if ( and  ( equal? #t ( Mover_Torre x y ) ) ( equal? ( Evitar_Salto x y ( char->integer ( string-ref x3 0 )) ( char->integer ( string-ref y3 0 )) add ) #t ))
                                                        ( misma_posicion? ) ( begin ( ( ( draw-pixmap-posn "invalido.png" ) Pantalla_De_Juego ) ( make-posn 715 350 ) )( Hacer_click1 )) )  ]
      [ ( equal? ( string-ref Posicion add ) #\C ) ( Hacer_click2 )
                                                   ( if ( equal? #t ( Mover_Caballo x y ) )  ( misma_posicion? ) ( Hacer_click1 ) ) ]
      [ ( equal? ( string-ref Posicion add ) #\A ) ( Hacer_click2 )
                                                   ( if ( and  ( equal? #t ( Mover_Alfil x y ) ) ( equal? ( Evitar_Salto x y ( char->integer ( string-ref x3 0 )) ( char->integer ( string-ref y3 0 )) add ) #t ))
                                                        ( misma_posicion? ) ( begin ( ( ( draw-pixmap-posn "invalido.png" ) Pantalla_De_Juego ) ( make-posn 715 350 ) )( Hacer_click1 )) ) ]
      [ ( equal? ( string-ref Posicion add ) #\R ) ( Hacer_click2 )
                                                   ( if ( and ( equal? #t ( Mover_Reina x y ) )( equal? ( Evitar_Salto x y ( char->integer ( string-ref x3 0 )) ( char->integer ( string-ref y3 0 )) add ) #t ))
                                                        ( misma_posicion? ) ( begin ( ( ( draw-pixmap-posn "invalido.png" ) Pantalla_De_Juego ) ( make-posn 715 350 ) )( Hacer_click1 )) ) ]
      [ ( equal? ( string-ref Posicion add ) #\K ) ( Hacer_click2 )
                                                   ( if ( and ( equal? #t ( Mover_Rey x y ) ) ( equal? ( Evitar_Salto x y ( char->integer ( string-ref x3 0 )) ( char->integer ( string-ref y3 0 )) add ) #t ))
                                                        ( misma_posicion_rey? ) ( begin ( ( ( draw-pixmap-posn "invalido.png" ) Pantalla_De_Juego ) ( make-posn 715 350 ) )( Hacer_click1 )) ) ]
      [ { or ( equal? ( string-ref Posicion add ) #\B )
             ( equal? ( string-ref Posicion add ) #\N )} ( Hacer_click2 )
                                                         ( if ( and ( equal? #t ( Mover_Peon add x y ) ) ( equal? ( Evitar_Salto x y ( char->integer ( string-ref x3 0 )) ( char->integer ( string-ref y3 0 )) add ) #t ))
                                                              ( misma_posicion? ) ( begin ( ( ( draw-pixmap-posn "invalido.png" ) Pantalla_De_Juego ) ( make-posn 715 350 ) )( Hacer_click1 )) ) ]
      [ else ( ( ( ( draw-pixmap-posn "invalido.png" ) Pantalla_De_Juego ) ( make-posn 715 350 ) ) ( Hacer_click1 )) ]
      );Final del cond
   )
;----------------------------------------------PINTA-CASILLAS-------------------------------------------------------------
( define ( pintar )
   ( if ( = ( remainder ( + ( char->integer ( string-ref x2 0 )) ( char->integer ( string-ref y2 0 )) ) 2 ) 0 )
        ( ( draw-solid-rectangle Pantalla_De_Juego )
          ( make-posn ( - ( * 75 ( char->integer ( string-ref x2 0 )) ) 25)
                      ( - ( * 75 ( char->integer ( string-ref y2 0 )) ) 25) ) 75 75 "MediumForestGreen"  )
        ( ( draw-solid-rectangle Pantalla_De_Juego )
          ( make-posn ( - ( * 75 ( char->integer ( string-ref x2 0 )) ) 25)
                      ( - ( * 75 ( char->integer ( string-ref y2 0 )) ) 25) ) 75 75 "Wheat" )
        )
   ( if ( = ( remainder ( + ( char->integer ( string-ref x3 0 )) ( char->integer ( string-ref y3 0 )) ) 2 ) 0 )
        ( ( draw-solid-rectangle Pantalla_De_Juego )
          ( make-posn ( - ( * 75 ( char->integer ( string-ref x3 0 )) ) 25)
                      ( - ( * 75 ( char->integer ( string-ref y3 0 )) ) 25) ) 75 75 "MediumForestGreen"  )
        ( ( draw-solid-rectangle Pantalla_De_Juego )
          ( make-posn ( - ( * 75 ( char->integer ( string-ref x3 0 )) ) 25)
                      ( - ( * 75 ( char->integer ( string-ref y3 0 )) ) 25) ) 75 75 "Wheat" )
        )
   );Final de la funcion pintar
;----------------------------------------------CAMBIA-DATOS---------------------------------------------------------------
( define ( Cambiar_posicion x y imagen add_Viejo add_Nuevo )
   ( pintar )
   ( ( ( draw-pixmap-posn imagen ) Pantalla_De_Juego ) ( make-posn ( * x 75 ) ( - ( * y 75 ) 20 ) ) )
   ( string-set! remplazado 0 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 16)) )
   ( string-set! remplazado 1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 15)) )
   ( string-set! Posicion add_Nuevo ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 ) ) 16)) )
   ( string-set! Posicion ( + 1 add_Nuevo ) ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 ) ) 15)) )
   ( string-set! Posicion add_Viejo #\- )
   ( string-set! Posicion ( + 1 add_Viejo) #\- )
   ( mostrar-jaque )
   ( Cambiar_turno ) ( Mostar_turno ) ( Hacer_click1 )
   );Final de la funcion Cambiar_posicion
( define ( Cambiar_posicion_salir_jaque x y imagen add_Viejo add_Nuevo )
   ( pintar )
   ( string-set! remplazado 0 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 16)) )
   ( string-set! remplazado 1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 15)) )
   ( ( ( draw-pixmap-posn imagen ) Pantalla_De_Juego ) ( make-posn ( * x 75 ) ( - ( * y 75 ) 20 ) ) )
   ( string-set! Posicion add_Nuevo ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 ) ) 16)) )
   ( string-set! Posicion ( + 1 add_Nuevo ) ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 ) ) 15)) )
   ( string-set! Posicion add_Viejo #\- )
   ( string-set! Posicion ( + 1 add_Viejo) #\- )
   ( mostrar-jaque )
   ( if ( and ( or ( equal? #t ( Revisar_casillas-blanco )) ( equal? #t ( Revisar_casillas-negro )) ))
        ( begin
           ( pintar )
           ( ( ( draw-pixmap-posn imagen ) Pantalla_De_Juego ) ( make-posn ( * ( char->integer ( string-ref x2 0 )) 75 ) ( - ( * ( char->integer ( string-ref y2 0 )) 75 ) 20 ) ) )
           ( string-set! Posicion add_Viejo ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 16)) )
           ( string-set! Posicion ( + 1 add_Viejo ) ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 15)) )
           ( string-set! Posicion add_Nuevo ( string-ref remplazado 0 ) )
           ( string-set! Posicion ( + 1 add_Nuevo) ( string-ref remplazado 1 ) )
           ( ( ( draw-pixmap-posn ( string-append ( make-string 1 ( string-ref remplazado 0 ) ) (make-string 1 ( string-ref remplazado 1 )) ".png" ) ) Pantalla_De_Juego ) ( make-posn ( * ( char->integer ( string-ref x3 0 )) 75 ) ( - ( * ( char->integer ( string-ref y3 0 )) 75 ) 20 ) ) )
           )
        (begin
          ( Cambiar_turno ) ( Mostar_turno ) ( Hacer_click1 )
          )
        )
   ( Hacer_click1 )
   );Final de la funcion Cambiar_posicion_salir_jaque
( define ( Cambiar_posicion_rey x y imagen add_Viejo add_Nuevo )
   ( pintar )
   ( ( ( draw-pixmap-posn imagen ) Pantalla_De_Juego ) ( make-posn ( * x 75 ) ( - ( * y 75 ) 20 ) ) )
   ( string-set! remplazado 0 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 16)) )
   ( string-set! remplazado 1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 15)) )
   ( string-set! Posicion add_Nuevo ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 ) ) 16)) )
   ( string-set! Posicion ( + 1 add_Nuevo ) ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 ) ) 15)) )
   ( string-set! Posicion add_Viejo #\- )
   ( string-set! Posicion ( + 1 add_Viejo) #\- )
   ( mostrar-jaque )
   (if ( = ( - ( char->integer ( string-ref add_nuevo 0 )) 16 ) add_Viejo )
       (void)
       ( if ( and ( or ( equal? #t ( Revisar_casillas-blanco )) ( equal? #t ( Revisar_casillas-negro )) ))
            ( begin
               ( pintar )
               ( ( ( draw-pixmap-posn imagen ) Pantalla_De_Juego ) ( make-posn ( * ( char->integer ( string-ref x2 0 )) 75 ) ( - ( * ( char->integer ( string-ref y2 0 )) 75 ) 20 ) ) )
               ( string-set! Posicion add_Viejo ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 16)) )
               ( string-set! Posicion ( + 1 add_Viejo ) ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 15)) )
               ( string-set! Posicion add_Nuevo ( string-ref remplazado 0 ) )
               ( string-set! Posicion ( + 1 add_Nuevo) ( string-ref remplazado 1 ) )
               ( ( ( draw-pixmap-posn ( string-append ( make-string 1 ( string-ref remplazado 0 ) ) (make-string 1 ( string-ref remplazado 1 )) ".png" ) ) Pantalla_De_Juego ) ( make-posn ( * ( char->integer ( string-ref x3 0 )) 75 ) ( - ( * ( char->integer ( string-ref y3 0 )) 75 ) 20 ) ) )
               )
            (begin
              ( Cambiar_turno ) ( Mostar_turno ) ( Hacer_click1 )
              )
            )
       )
   ( Hacer_click1 )
   );Final de la funcion Cambiar_posicion_rey
;----------------------------------------------MOVIMIENTO-DE-LAS-FICHAS--------------------------------------------------------
;TORRE
( define ( Mover_Torre x y )
   ( if ( and ( equal? ( Come_el_rey? ) #f )
              ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                   ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
              ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                   ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                   ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
              ( or ( = x ( char->integer ( string-ref x3 0 )) ) ( = y ( char->integer ( string-ref y3 0 )) ) ))
        #t
        #f
        );Final del if
   )
;CABALLO
( define ( Mover_Caballo x y )
   ( if    ( and ( equal? ( Come_el_rey? ) #f )
                 ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                      ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
                 ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                      ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                      ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
                 ( or ( and ( = ( + x 1 ) ( char->integer ( string-ref x3 0 )) ) ( = ( - y 2 ) ( char->integer ( string-ref y3 0 )) ) )
                      ( and ( = ( - x 1 ) ( char->integer ( string-ref x3 0 )) ) ( = ( - y 2 ) ( char->integer ( string-ref y3 0 )) ) )
                      ( and ( = ( + x 1 ) ( char->integer ( string-ref x3 0 )) ) ( = ( + y 2 ) ( char->integer ( string-ref y3 0 )) ) )
                      ( and ( = ( - x 1 ) ( char->integer ( string-ref x3 0 )) ) ( = ( + y 2 ) ( char->integer ( string-ref y3 0 )) ) )
             ( and ( = ( + x 2 ) ( char->integer ( string-ref x3 0 )) ) ( = ( - y 1 ) ( char->integer ( string-ref y3 0 )) ) )
             ( and ( = ( - x 2 ) ( char->integer ( string-ref x3 0 )) ) ( = ( - y 1 ) ( char->integer ( string-ref y3 0 )) ) )
             ( and ( = ( + x 2 ) ( char->integer ( string-ref x3 0 )) ) ( = ( + y 1 ) ( char->integer ( string-ref y3 0 )) ) )
             ( and ( = ( - x 2 ) ( char->integer ( string-ref x3 0 )) ) ( = ( + y 1 ) ( char->integer ( string-ref y3 0 )) ) ) ))
           #t
           #f
           
           )
   )
;ALFIL
( define ( Mover_Alfil x y )
   ( if ( and ( equal? ( Come_el_rey? ) #f )
              ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                   ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
              ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                   ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                   ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
              ( = ( Valor_Absoluto ( - x ( char->integer ( string-ref x3 0 )))) ( Valor_Absoluto ( - y ( char->integer ( string-ref y3 0 )))) ) )
        #t
        #f
        )
   )
;REINA
( define ( Mover_Reina x y )
   ( if ( and ( equal? ( Come_el_rey? ) #f )
              ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                   ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
              ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                   ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                   ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
              ( or ( and
                     ( = ( Valor_Absoluto ( - x ( char->integer ( string-ref x3 0 ))) ) ( Valor_Absoluto ( - y ( char->integer ( string-ref y3 0 )))) ))
                   ( = x ( char->integer ( string-ref x3 0 )) ) ( = y ( char->integer ( string-ref y3 0 )) ) ) )
        #t
        #f
        )
   )
;REY
( define ( Mover_Rey x y )
   ( if ( and ( equal? ( Come_el_rey? ) #f )
              ( >= 18 ( - ( char->integer ( string-ref add_nuevo 0 )) ( char->integer ( string-ref add_viejo 0 )) ))
              ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                   ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
              ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                   ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                   ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
              ( or ( and
                     ( = ( Valor_Absoluto ( - x ( char->integer ( string-ref x3 0 ))) ) ( Valor_Absoluto ( - y ( char->integer ( string-ref y3 0 )))) ))
                   ( = x ( char->integer ( string-ref x3 0 )) ) ( = y ( char->integer ( string-ref y3 0 )) ) )
              ( or  ( = ( + x 1 ) ( char->integer ( string-ref x3 0 )) ) ( = ( + y 1 ) ( char->integer ( string-ref y3 0 )) )
                    ( = ( - x 1 ) ( char->integer ( string-ref x3 0 )) ) ( = ( - y 1 ) ( char->integer ( string-ref y3 0 )) )
                    ))
        #t
        #f
        )
   )
;PEON
( define ( Mover_Peon add x y )
   ( if ( equal? ( string-ref Posicion add ) #\N )
        ( cond
           [ ( and  ( <= add 30 ) ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                                       ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
                    ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                         ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                         ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
                    ( equal? #\- ( string-ref Posicion ( + add 16 ) ) )
                    ( = x ( char->integer ( string-ref x3 0 )) )
                    ( = ( + y 1 ) ( char->integer ( string-ref y3 0 )) ) )
             ( Coronacion? x y )
             ]
           [ ( and  ( <= add 30 ) ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                                       ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
                    ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                         ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                         ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
                    ( equal? #\- ( string-ref Posicion ( + add 32 ) ) )
                    ( = x ( char->integer ( string-ref x3 0 )) )
                    ( = ( + y 2 ) ( char->integer ( string-ref y3 0 )) ) )
             ( Coronacion? x y )
             ]
           [ ( and ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                        ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
                   ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                        ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                        ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
                   ( equal? #\- ( string-ref Posicion ( + add 16 ) ) )
                   ( = x ( char->integer ( string-ref x3 0 )) )
                   ( = ( + y 1 ) ( char->integer ( string-ref y3 0 )) ) )
             ( Coronacion? x y )
              ]
           ;Para comer blancas
           [ ( and  ( equal? ( Come_el_rey? ) #f )
                    ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                         ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
                    ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                         ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                         ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
                    ( = ( + x 1 ) ( char->integer ( string-ref x3 0 )) )
                    ( = ( + y 1 ) ( char->integer ( string-ref y3 0 )) )
                    ( equal? #\1 ( string-ref Posicion ( + add 19 ) ) ))
             ( Coronacion? x y )
             ]
           [ ( and ( equal? ( Come_el_rey? ) #f )
                   ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                        ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
                   ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                        ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                        ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
                   ( = ( - x 1 ) ( char->integer ( string-ref x3 0 )) )
                   ( = ( + y 1 ) ( char->integer ( string-ref y3 0 )) )
                   ( equal? #\1 ( string-ref Posicion ( + add 15 ) ) ))
             ( Coronacion? x y )
             ]
           ( else #f )
           );Final del cond
        ( cond
           [ ( and  ( >= add 96 ) ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                                       ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
                    ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                         ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                         ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
                    ( equal? #\- ( string-ref Posicion ( - add 16 ) ) )
                    ( = x ( char->integer ( string-ref x3 0 )) )
                    ( = ( - y 1 ) ( char->integer ( string-ref y3 0 )) ) )
             ( Coronacion? x y )
             ]
           [ ( and  ( >= add 96 ) ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                                       ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
                    ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                         ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                         ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
                    ( equal? #\- ( string-ref Posicion ( - add 32 ) ) )
                    ( = x ( char->integer ( string-ref x3 0 )) )
                    ( = ( - y 2 ) ( char->integer ( string-ref y3 0 )) ) )
             ( Coronacion? x y )
             ]
           [ ( and ( or ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                        ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 )) 15 ) ) ) ( equal? turno "2" ) ))
                   ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
               ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
               ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
                   ( equal? #\- ( string-ref Posicion ( - add 16 ) ) )
                   ( = x ( char->integer ( string-ref x3 0 )) )
                   ( = ( - y 1 ) ( char->integer ( string-ref y3 0 )) ) )
             ( Coronacion? x y )
             ]
           ;Para comer negras.
           [ ( and ( equal? ( Come_el_rey? ) #f )
                   ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                        ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                        ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
                   ( = ( - x 1 ) ( char->integer ( string-ref x3 0 )) )
                   ( = ( - y 1 ) ( char->integer ( string-ref y3 0 )) )
                   ( equal? #\2 ( string-ref Posicion ( - add 17 ) ) ))
             ( Coronacion? x y )
             ]
           [ ( and  ( equal? ( Come_el_rey? ) #f )
                    ( or ( and ( equal? #\2 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "1" ) )
                         ( and ( equal? #\1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ) ( equal? turno "2" ) )
                         ( equal? #\- ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 )) 15 ) ) ))
                    ( = ( + x 1 ) ( char->integer ( string-ref x3 0 )) )
                    ( = ( - y 1 ) ( char->integer ( string-ref y3 0 )) )
                    ( equal? #\2 ( string-ref Posicion ( - add 13 ) ) ))
             ( Coronacion? x y )
             ]
           ( else #f )
           );FInal del cond
        );Final del if
   )
;-----------------------------------------MOSTRAR-TURNO------------------------------------------------------------------------
( define ( Mostar_turno )
   ( if ( equal? turno "1" )
        ( ( ( draw-pixmap-posn "Turno1.png" ) Pantalla_De_Juego ) ( make-posn 680 25 ) )
        ( ( ( draw-pixmap-posn "Turno2.png" ) Pantalla_De_Juego ) ( make-posn 680 25 ) )
        );Final del if
   );Final de la funcion Mostrar_turno
;-----------------------------------------REALIZAR-CORONACION------------------------------------------------------------------
( define ( Coronacion? x y )
   ( string-set! x2 0 ( integer->char x ) )
        ( string-set! y2 0 ( integer->char y ) )
   ( cond
      ;Para coronar Blancas
      [ ( and  ( equal? ( Come_el_rey? ) #f )
               ( equal? turno "1" )
               ( = ( - y 1 ) ( char->integer ( string-ref y3 0 )) )
               ( = 1 ( char->integer ( string-ref y3 0 )) ) )
        ( Abrir-coronacion ) ]
      ;Para coronar Negras
      [ ( and  ( equal? ( Come_el_rey? ) #f )
               ( equal? turno "2" )
               ( = ( + y 1 ) ( char->integer ( string-ref y3 0 )) )
               ( = 8 ( char->integer ( string-ref y3 0 )) ) )
        ( Abrir-coronacion ) ]
      [ else #t ]
      );Final del cond
   );Final de la funcion Coronacion?
;-----------------------------------------CLICK-DE-LA-CORONACION---------------------------------------------------------------
( define ( Abrir-coronacion )
   ( define Cambiar_peon (open-viewport "Coronación" 400 200 ) )
   ( define ( Hacer_click3 )
      ( define Click3 ( get-mouse-click Cambiar_peon ) )
      ( if ( equal? turno "1" )
           ( cond
              [[and (>= (posn-x (mouse-click-posn Click3)) 50 )
                    (>=(posn-y (mouse-click-posn Click3)) 125 )
                    (<=(posn-x (mouse-click-posn Click3)) 125 )
                    (<=(posn-y (mouse-click-posn Click3)) 200 )]
               ( Coronacion "T1.png" ) ]
              [[and (>= (posn-x (mouse-click-posn Click3)) 126 )
                    (>=(posn-y (mouse-click-posn Click3)) 125 )
                    (<=(posn-x (mouse-click-posn Click3)) 200 )
                    (<=(posn-y (mouse-click-posn Click3)) 200 )]
               ( Coronacion "A1.png" ) ]
              [[and (>= (posn-x (mouse-click-posn Click3)) 201 )
                    (>=(posn-y (mouse-click-posn Click3)) 125 )
                    (<=(posn-x (mouse-click-posn Click3)) 275 )
                    (<=(posn-y (mouse-click-posn Click3)) 200 )]
               ( Coronacion "C1.png" ) ]
              [[and (>= (posn-x (mouse-click-posn Click3)) 276 )
                    (>=(posn-y (mouse-click-posn Click3)) 125 )
                    (<=(posn-x (mouse-click-posn Click3)) 350 )
                    (<=(posn-y (mouse-click-posn Click3)) 200 )]
               ( Coronacion "R1.png" ) ]
              [ else( Hacer_click3 ) ]
              );Final del cond
           ( cond
              [[and (>= (posn-x (mouse-click-posn Click3)) 50 )
                    (>=(posn-y (mouse-click-posn Click3)) 125 )
                    (<=(posn-x (mouse-click-posn Click3)) 125 )
                    (<=(posn-y (mouse-click-posn Click3)) 200 )]
               ( Coronacion "T2.png" ) ]
              [[and (>= (posn-x (mouse-click-posn Click3)) 126 )
                    (>=(posn-y (mouse-click-posn Click3)) 125 )
                    (<=(posn-x (mouse-click-posn Click3)) 200 )
                    (<=(posn-y (mouse-click-posn Click3)) 200 )]
               ( Coronacion "A2.png" ) ]
              [[and (>= (posn-x (mouse-click-posn Click3)) 201 )
                    (>=(posn-y (mouse-click-posn Click3)) 125 )
                    (<=(posn-x (mouse-click-posn Click3)) 275 )
                    (<=(posn-y (mouse-click-posn Click3)) 200 )]
               ( Coronacion "C2.png" ) ]
              [[and (>= (posn-x (mouse-click-posn Click3)) 276 )
                    (>=(posn-y (mouse-click-posn Click3)) 125 )
                    (<=(posn-x (mouse-click-posn Click3)) 350 )
                    (<=(posn-y (mouse-click-posn Click3)) 200 )]
               ( Coronacion "R2.png" ) ]
              [ else( Hacer_click3 ) ]
              );Final del cond
           );Final del if
      );Final de la funcio hacer click
;-----------------------------------------CAMBIAR-EL-PEON----------------------------------------------------------------------
   ( define ( Coronacion imagen )
      ( pintar )
      ( ( ( draw-pixmap-posn imagen ) Pantalla_De_Juego ) ( make-posn ( * 75 ( char->integer ( string-ref x3 0 ))) ( - ( * 75 ( char->integer ( string-ref y3 0 ))) 20 ) ) )
      ( string-set! Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 16) ( string-ref imagen 0 ) )
      ( string-set! Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 15) ( string-ref imagen 1 ) )
      ( string-set! Posicion ( - ( char->integer ( string-ref add_viejo 0 ) ) 16) #\- )
      ( string-set! Posicion ( - ( char->integer ( string-ref  add_viejo 0 ) ) 15 ) #\- )
      ( Cambiar_turno ) ( Mostar_turno ) ( close-viewport Cambiar_peon )
      ( Hacer_click1 )
      );Final de la funcion Coronacion
   
;-----------------------------------------VENTANA-DE-CORONACION----------------------------------------------------------------
   ( ( ( draw-pixmap-posn "Menu.png" ) Cambiar_peon ) ( make-posn -50 -60 ) )
   ( ( ( draw-pixmap-posn "Texto.png" ) Cambiar_peon ) ( make-posn 10 80 ) )
   ( if ( equal? turno "1" )
        ( begin
           ( ( ( draw-pixmap-posn "T1.png" ) Cambiar_peon ) ( make-posn 50 125 ) )
           ( ( ( draw-pixmap-posn "A1.png" ) Cambiar_peon ) ( make-posn 140 125 ) )
           ( ( ( draw-pixmap-posn "C1.png" ) Cambiar_peon ) ( make-posn 215 125 ) )
           ( ( ( draw-pixmap-posn "R1.png" ) Cambiar_peon ) ( make-posn 315 125 ) )
           )
        ( begin
           ( ( ( draw-pixmap-posn "T2.png" ) Cambiar_peon ) ( make-posn 50 125 ) )
           ( ( ( draw-pixmap-posn "A2.png" ) Cambiar_peon ) ( make-posn 140 125 ) )
           ( ( ( draw-pixmap-posn "C2.png" ) Cambiar_peon ) ( make-posn 215 125 ) )
           ( ( ( draw-pixmap-posn "R2.png" ) Cambiar_peon ) ( make-posn 315 125 ) )
           )
        );Final del if
   ( Hacer_click3 )
   );Final de la funcion Abrir-coronacion
;----------------------------------------------JAQUE---------------------------------------------------------------------------
( define ( Revisar_casillas-blanco )
   ( cond
      [ ( equal? #t ( Revisar_peones_Blancos 0 ) )  #t ]
      [ ( equal? #t ( Revisar_caballos_Blancos 0 )) #t ]
      [ ( equal? #t ( Jaque-Torres-parte-reinas-blanco )) #t ]
      [ ( equal? #t ( Jaque-alfil-parte-reinas-blanco )) #t ]
      [ else #f ]
      );Final del cond.
   );Final de la funcion Revisar_casillas-blanco
( define ( Revisar_casillas-negro )
   ( cond
      [ ( equal? #t ( Revisar_peones_Negros 0 )) #t ]
      [ ( equal? #t ( Revisar_caballos_Negros 0  )) #t ]
      [ ( equal? #t ( Jaque-Torres-parte-reinas-negro )) #t ]
      [ ( equal? #t ( Jaque-alfil-parte-reinas-negro )) #t ]
      [ else #f ]
      );Final del cond.
   );Final de la funcion Revisar_casillas-negro
( define ( Revisar_peones_Blancos inicio  )
   ( if ( < inicio 128 )
        ( if ( and ( equal? ( string-ref Posicion inicio ) #\B ))
             ( begin
                ( cond
                   (( > inicio 12 ) (cond
                                      (( and ( equal? #\K ( string-ref Posicion ( - inicio 14 ) ) )
                                            ( equal? #\2 ( string-ref Posicion ( - inicio 13 ) ) ) )
                                      #t)
                                      ( else ( Revisar_peones_Blancos ( + 2 inicio) ))))
                   (( > inicio 16 ) (cond
                                      (( and ( equal? #\K ( string-ref Posicion ( - inicio 18 ) ) )
                                            ( equal? #\2 ( string-ref Posicion ( - inicio 17 ) ) ) )
                                      #t)
                                      ( else ( Revisar_peones_Blancos ( + 2 inicio) ))))
                   ( else ( Revisar_peones_Blancos ( + 2 inicio) ))
                     )
                )
             ( Revisar_peones_Blancos ( + 2 inicio) ))
        (void)
        );Final del if
   );Final de la funcion Revisar_peones
( define ( Revisar_peones_Negros inicio  )
   ( if ( < inicio 128 )
        ( if ( equal? ( string-ref Posicion inicio ) #\N )
             ( begin
                ( cond
                   (( < inicio 109 ) (cond
                                      (( and ( equal? #\K ( string-ref Posicion ( + inicio 18 ) ) )
                                             ( equal? #\1 ( string-ref Posicion ( + inicio 19 ) ) ) )
                                      #t)
                                      ( else ( Revisar_peones_Negros ( + 2 inicio) ))))
                   (( < inicio 113 ) (cond
                                      (( and ( equal? #\K ( string-ref Posicion ( + inicio 14 ) ) )
                                            ( equal? #\1 ( string-ref Posicion ( + inicio 15 ) ) ) )
                                      #t)
                                      ( else ( Revisar_peones_Negros ( + 2 inicio) ))))
                   ( else ( Revisar_peones_Negros ( + 2 inicio) ))
                     )
                )
             ( Revisar_peones_Negros ( + 2 inicio) ))
        (void)
        );Final del if
   );Final de la funcion Revisar_peones
( define ( Revisar_caballos_Blancos inicio )
   ( if ( < inicio 128 )
        ( if ( and ( equal? ( string-ref Posicion inicio ) #\C ) ( equal? ( string-ref Posicion ( + 1 inicio ) ) #\1 ) )
             ( begin
                ( if ( or 
                       ( and ( >= inicio 20 )( equal? #\K ( string-ref Posicion ( - inicio 20 ) ) )
                             ( equal? #\2 ( string-ref Posicion ( - inicio 19 ) ) ) )
                       ( and ( >= inicio 12 )( equal? #\K ( string-ref Posicion ( - inicio 12 ) ) )
                             ( equal? #\2 ( string-ref Posicion ( - inicio 11 ) ) ) )
                       ( and ( >= inicio 34 )( equal? #\K ( string-ref Posicion ( - inicio 34 ) ) )
                             ( equal? #\2 ( string-ref Posicion ( - inicio 33 ) ) ) )
                       ( and ( >= inicio 30 )( equal? #\K ( string-ref Posicion ( - inicio 30 ) ) )
                             ( equal? #\2 ( string-ref Posicion ( - inicio 29 ) ) ) )
                       ( and ( <= inicio 108 )( equal? #\K ( string-ref Posicion ( + inicio 20 ) ) )
                             ( equal? #\2 ( string-ref Posicion ( + inicio 21 ) ) ) )
                       ( and ( <= inicio 116 )( equal? #\K ( string-ref Posicion ( + inicio 12 ) ) )
                             ( equal? #\2 ( string-ref Posicion ( + inicio 13 ) ) ) )
                       ( and ( < inicio 94 )( equal? #\K ( string-ref Posicion ( + inicio 34 ) ) )
                             ( equal? #\2 ( string-ref Posicion ( + inicio 35 ) ) ) )
                       ( and ( <= inicio 98 )( equal? #\K ( string-ref Posicion ( + inicio 30 ) ) )
                             ( equal? #\2 ( string-ref Posicion ( + inicio 31 ) ) ) )
                       )
                #t
                ( Revisar_caballos_Blancos ( + 2 inicio) )
                 )
               )
             ( Revisar_caballos_Blancos ( + 2 inicio) ))
        (void)
        );Final del if
   );Final de la funcion Revisar_caballos_Blancos
( define ( Revisar_caballos_Negros inicio )
   ( if ( < inicio 128 )
        ( if ( and ( equal? ( string-ref Posicion inicio ) #\C ) ( equal? ( string-ref Posicion ( + 1 inicio ) ) #\2 ) )
             ( begin
                ( if ( or 
                       ( and ( >= inicio 20 )( equal? #\K ( string-ref Posicion ( - inicio 20 ) ) )
                             ( equal? #\1 ( string-ref Posicion ( - inicio 19 ) ) ) )
                       ( and ( >= inicio 12 )( equal? #\K ( string-ref Posicion ( - inicio 12 ) ) )
                             ( equal? #\1 ( string-ref Posicion ( - inicio 11 ) ) ) )
                       ( and ( >= inicio 34 )( equal? #\K ( string-ref Posicion ( - inicio 34 ) ) )
                             ( equal? #\1 ( string-ref Posicion ( - inicio 33 ) ) ) )
                       ( and ( >= inicio 30 )( equal? #\K ( string-ref Posicion ( - inicio 30 ) ) )
                             ( equal? #\1 ( string-ref Posicion ( - inicio 29 ) ) ) )
                       ( and ( <= inicio 108 )( equal? #\K ( string-ref Posicion ( + inicio 20 ) ) )
                             ( equal? #\1 ( string-ref Posicion ( + inicio 21 ) ) ) )
                       ( and ( <= inicio 116 )( equal? #\K ( string-ref Posicion ( + inicio 12 ) ) )
                             ( equal? #\1 ( string-ref Posicion ( + inicio 13 ) ) ) )
                       ( and ( <= inicio 94 )( equal? #\K ( string-ref Posicion ( + inicio 34 ) ) )
                             ( equal? #\1 ( string-ref Posicion ( + inicio 35 ) ) ) )
                       ( and ( <= inicio 98 )( equal? #\K ( string-ref Posicion ( + inicio 30 ) ) )
                             ( equal? #\1 ( string-ref Posicion ( + inicio 31 ) ) ) )
                       )
                     #t
                     ( Revisar_caballos_Negros ( + 2 inicio) )
                     )
                )
             ( Revisar_caballos_Negros ( + 2 inicio) ))
        (void)
        );Final del if
   );Final de la funcion Revisar_caballos_Negros
( define ( Jaque-Torres-parte-reinas-blanco )
   ( cond
      [( equal? #t ( Revisar_salto_Torre_Blanca_abajo ( char->integer ( string-ref add-rey-negro 0 ))  ))#t]
      [( equal? #t ( Revisar_salto_Torre_Blanca_arriba ( char->integer ( string-ref add-rey-negro 0 ))  ))#t]
      [( equal? #t ( Revisar_salto_Torre_Blanca_derecha ( char->integer ( string-ref add-rey-negro 0 ))  ))#t]
      [( equal? #t ( Revisar_salto_Torre_Blanca_izquierda ( char->integer ( string-ref add-rey-negro 0 ))  ))#t]
      [ else #f ]
      )
   )
( define ( Jaque-Torres-parte-reinas-negro )
   ( cond
      [( equal? #t ( Revisar_salto_Torre_Negra_abajo ( char->integer ( string-ref add-rey-blanco 0 ))  ))#t]
      [( equal? #t ( Revisar_salto_Torre_Negra_arriba ( char->integer ( string-ref add-rey-blanco 0 ))  ))#t]
      [( equal? #t ( Revisar_salto_Torre_Negra_derecha ( char->integer ( string-ref add-rey-blanco 0 ))  ))#t]
      [( equal? #t ( Revisar_salto_Torre_Negra_izquierda ( char->integer ( string-ref add-rey-blanco 0 ))  ))#t]
      [ else #f ]
      )
   )
( define ( Revisar_salto_Torre_Blanca_abajo add  )
   ( cond
      ;Rey-mira-abajo
      [ ( and  ( <= add 111 ) ( equal? ( string-ref Posicion ( + add 16 ) ) #\- ) ) ( Revisar_salto_Torre_Blanca_abajo ( + add 16 ) )  ]
      [ ( and  ( <= add 111 ) ( or ( equal? ( string-ref Posicion ( + add 16 ) ) #\R ) ( equal? ( string-ref Posicion ( + add 16 ) ) #\T )) ( equal? ( string-ref Posicion ( + add 17 ) ) #\1 ) ) #t ]
      )
   )
( define ( Revisar_salto_Torre_Blanca_arriba add  )
   ( cond
      ;Rey-mira-arriba
      [ ( and  ( >= add 16 ) ( equal? ( string-ref Posicion ( - add 16 ) ) #\- ) ) ( Revisar_salto_Torre_Blanca_arriba ( - add 16 )  )  ]
      [ ( and  ( >= add 16 ) ( or ( equal? ( string-ref Posicion ( - add 16 ) ) #\R ) ( equal? ( string-ref Posicion ( - add 16 ) ) #\T )) ( equal? ( string-ref Posicion ( - add 15 ) ) #\1 ) ) #t ]
      )
   )
( define ( Revisar_salto_Torre_Blanca_derecha add  )
   ( cond
      ;Rey-mira-derecha
      [ ( or ( = add 0 ) ( = add 16 ) ( = add 32 ) ( = add 48 )  ( = add 64 ) ( = add 80 ) ( = add 96 ) ( = add 112 )) #f ] 
      [ ( and  ( < add 127 ) ( equal? ( string-ref Posicion ( + add 2 ) ) #\- ) ) ( Revisar_salto_Torre_Blanca_derecha ( + add 2 )  )  ]
      [ ( and  ( < add 127 ) ( or ( equal? ( string-ref Posicion ( + add 2 ) ) #\R ) ( equal? ( string-ref Posicion ( + add 2 ) ) #\T )) ( equal? ( string-ref Posicion ( + add 3 ) ) #\1 ) ) #t ]
      )
   )
( define ( Revisar_salto_Torre_Blanca_izquierda add  )
   ( cond
      ;Rey-mira-izquierda
      [ ( or ( = add 14 ) ( = add 30 ) ( = add 46 ) ( = add 62 )  ( = add 78 ) ( = add 94 ) ( = add 110 ) ( = add 126 )) #f ]
      [ ( and  ( > add 2 ) ( equal? ( string-ref Posicion ( - add 2 ) ) #\- ) ) ( Revisar_salto_Torre_Blanca_izquierda ( - add 2 )  )  ]
      [ ( and  ( > add 2 ) ( or ( equal? ( string-ref Posicion ( - add 2 ) ) #\R ) ( equal? ( string-ref Posicion ( - add 2 ) ) #\T ) ) ( equal? ( string-ref Posicion ( - add 1 ) ) #\1 ) ) #t ]
      )
   )
( define ( Revisar_salto_Torre_Negra_abajo add  )
   ( cond
      ;Rey-mira-abajo
      [ ( and ( <= add 111 ) ( equal? ( string-ref Posicion ( + add 16 ) ) #\- ) ) ( Revisar_salto_Torre_Negra_abajo ( + add 16 ) ) ]
      [ ( and ( <= add 111 ) ( or ( equal? ( string-ref Posicion ( + add 16 ) ) #\R ) ( equal? ( string-ref Posicion ( + add 16 ) ) #\T )) ( equal? ( string-ref Posicion ( + add 17 ) ) #\2 ) ) #t ]
      )
   )
( define ( Revisar_salto_Torre_Negra_arriba add  )
   ( cond
      ;Rey-mira-arriba
      [ ( and  ( >= add 16 ) ( equal? ( string-ref Posicion ( - add 16 ) ) #\- ) ) ( Revisar_salto_Torre_Negra_arriba ( - add 16 )  ) ]
      [ ( and  ( >= add 16 ) ( or ( equal? ( string-ref Posicion ( - add 16 ) ) #\R ) ( equal? ( string-ref Posicion ( - add 16 ) ) #\T )) ( equal? ( string-ref Posicion ( - add 15 ) ) #\2 ) ) #t ]
      )
   )
( define ( Revisar_salto_Torre_Negra_derecha add  )
   ( cond
      ;Rey-mira-derecha
      [ ( or ( = add 0 ) ( = add 16 ) ( = add 32 ) ( = add 48 )  ( = add 64 ) ( = add 80 ) ( = add 96 ) ( = add 112 )) #f ]
      [ ( and  ( < add 127 ) ( equal? ( string-ref Posicion ( + add 2 ) ) #\- ) ) ( Revisar_salto_Torre_Negra_derecha ( + add 2 )  ) ]
      [ ( and  ( < add 127 ) ( or ( equal? ( string-ref Posicion ( + add 2 ) ) #\R ) ( equal? ( string-ref Posicion ( + add 2 ) ) #\T )) ( equal? ( string-ref Posicion ( + add 3 ) ) #\2 ) ) #t ]
      )
   )
( define ( Revisar_salto_Torre_Negra_izquierda add  )
   ( cond
      ;Rey-mira-izquierda
      [ ( or ( = add 14 ) ( = add 30 ) ( = add 46 ) ( = add 62 )  ( = add 78 ) ( = add 94 ) ( = add 110 ) ( = add 126 )) #f ]
      [ ( and  ( > add 2 ) ( equal? ( string-ref Posicion ( - add 2 ) ) #\- ) ) ( Revisar_salto_Torre_Negra_izquierda ( - add 2 )  ) ]
      [ ( and  ( > add 2 ) ( or ( equal? ( string-ref Posicion ( - add 2 ) ) #\R ) ( equal? ( string-ref Posicion ( - add 2 ) ) #\T ) ) ( equal? ( string-ref Posicion ( - add 1 ) ) #\2 ) ) #t ]
      )
   )
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DIAGONALES
( define ( Jaque-alfil-parte-reinas-blanco )
   ( cond
      [( equal? #t ( Revisar_salto_alfil_Blanco_abajo_derecha ( char->integer ( string-ref add-rey-negro 0 ))  )) #t ]
      [( equal? #t ( Revisar_salto_alfil_Blanco_abajo_izquierda ( char->integer ( string-ref add-rey-negro 0 ))  ))#t]
      [( equal? #t ( Revisar_salto_alfil_Blanco_arriba_derecha ( char->integer ( string-ref add-rey-negro 0 ))  ))#t]
      [( equal? #t ( Revisar_salto_alfil_Blanco_arriba_izquierda ( char->integer ( string-ref add-rey-negro 0 ))  ))#t]
      )
   )
( define ( Jaque-alfil-parte-reinas-negro )
   ( cond
      [( equal? #t ( Revisar_salto_alfil_Negro_abajo_derecha ( char->integer ( string-ref add-rey-blanco 0 ))  ))#t]
      [( equal? #t ( Revisar_salto_alfil_Negro_abajo_izquierda ( char->integer ( string-ref add-rey-blanco 0 ))  ))#t]
      [( equal? #t ( Revisar_salto_alfil_Negro_arriba_derecha ( char->integer ( string-ref add-rey-blanco 0 ))  ))#t]
      [( equal? #t ( Revisar_salto_alfil_Negro_arriba_izquierda ( char->integer ( string-ref add-rey-blanco 0 ))  ))#t]
      )
   )
( define ( Revisar_salto_alfil_Blanco_abajo_derecha add  )
   ( cond
      ;Rey-mira-abajo
      [ ( or ( = add 0 ) ( = add 16 ) ( = add 32 ) ( = add 48 )  ( = add 64 ) ( = add 80 ) ( = add 96 ) ( = add 112 )) #f ]
      [ ( and ( <= add 109 ) ( equal? ( string-ref Posicion ( + add 18 ) ) #\- ) )  ( Revisar_salto_alfil_Blanco_abajo_derecha ( + add 18 ) ) ]
      [ ( and ( <= add 109 ) ( or ( equal? ( string-ref Posicion ( + add 18 ) ) #\R ) ( equal? ( string-ref Posicion ( + add 18 ) ) #\A )) ( equal? ( string-ref Posicion ( + add 19 ) ) #\1 ) ) #t ]
      ))
( define ( Revisar_salto_alfil_Blanco_abajo_izquierda add  )
   ( cond
      ;Rey-mira-abajo
      [ ( or ( = add 14 ) ( = add 30 ) ( = add 46 ) ( = add 62 )  ( = add 78 ) ( = add 94 ) ( = add 110 ) ( = add 126 )) #f ]
      [ ( and  ( <= add 113 ) ( equal? ( string-ref Posicion ( + add 14 ) ) #\- ) )  ( Revisar_salto_alfil_Blanco_abajo_izquierda ( + add 14 ) ) ]
      [ ( and   ( <= add 113 ) ( or ( equal? ( string-ref Posicion ( + add 14 ) ) #\R ) ( equal? ( string-ref Posicion ( + add 14 ) ) #\A )) ( equal? ( string-ref Posicion ( + add 15 ) ) #\1 ) ) #t ]
      ))
( define ( Revisar_salto_alfil_Blanco_arriba_derecha add  )
   ( cond
      ;Rey-mira-arriba
 
      [ ( or ( = add 0 ) ( = add 16 ) ( = add 32 ) ( = add 48 )  ( = add 64 ) ( = add 80 ) ( = add 96 ) ( = add 112 )) #f ]
      [ ( and ( >= add 14 ) ( equal? ( string-ref Posicion ( - add 14 ) ) #\- ) )  ( Revisar_salto_alfil_Blanco_arriba_derecha ( - add 14 ) ) ]
      [ ( and ( >= add 14 ) ( or ( equal? ( string-ref Posicion ( - add 14 ) ) #\R ) ( equal? ( string-ref Posicion ( - add 14 ) ) #\A )) ( equal? ( string-ref Posicion ( - add 13 ) ) #\1 ) ) #t ]
      ))
( define ( Revisar_salto_alfil_Blanco_arriba_izquierda add  )
   ( cond
      ;Rey-mira-arriba
      
      [ ( or ( = add 14 ) ( = add 30 ) ( = add 46 ) ( = add 62 )  ( = add 78 ) ( = add 94 ) ( = add 110 ) ( = add 126 )) #f ]
      [ ( and  ( >= add 18 ) ( equal? ( string-ref Posicion ( - add 18 ) ) #\- ) )  ( Revisar_salto_alfil_Blanco_arriba_izquierda ( - add 18 ) ) ]
      [ ( and   ( >= add 18 ) ( or ( equal? ( string-ref Posicion ( - add 18 ) ) #\R ) ( equal? ( string-ref Posicion ( - add 18 ) ) #\A )) ( equal? ( string-ref Posicion ( - add 17 ) ) #\1 ) ) #t ]
      ))
( define ( Revisar_salto_alfil_Negro_abajo_derecha add  )
   ( cond
      ;Rey-mira-abajo
      
      [ ( or ( = add 0 ) ( = add 16 ) ( = add 32 ) ( = add 48 )  ( = add 64 ) ( = add 80 ) ( = add 96 ) ( = add 112 )) #f ]
      [ ( and ( <= add 109 ) ( equal? ( string-ref Posicion ( + add 18 ) ) #\- ) )  ( Revisar_salto_alfil_Negro_abajo_derecha ( + add 18 ) ) ]
      [ ( and ( <= add 109 ) ( or ( equal? ( string-ref Posicion ( + add 18 ) ) #\R ) ( equal? ( string-ref Posicion ( + add 18 ) ) #\A )) ( equal? ( string-ref Posicion ( + add 19 ) ) #\2 ) ) #t ]
      ))
( define ( Revisar_salto_alfil_Negro_abajo_izquierda add  )
   ( cond
      ;Rey-mira-abajo
      
      [ ( or ( = add 14 ) ( = add 30 ) ( = add 46 ) ( = add 62 )  ( = add 78 ) ( = add 94 ) ( = add 110 ) ( = add 126 )) #f ]
      [ ( and  ( <= add 113 ) ( equal? ( string-ref Posicion ( + add 14 ) ) #\- ) )  ( Revisar_salto_alfil_Negro_abajo_izquierda ( + add 14 ) ) ]
      [ ( and   ( <= add 113 ) ( or ( equal? ( string-ref Posicion ( + add 14 ) ) #\R ) ( equal? ( string-ref Posicion ( + add 14 ) ) #\A )) ( equal? ( string-ref Posicion ( + add 15 ) ) #\2 ) ) #t ]
      ))
( define ( Revisar_salto_alfil_Negro_arriba_derecha add  )
   ( cond
      ;Rey-mira-arriba
      
      [ ( or ( = add 0 ) ( = add 16 ) ( = add 32 ) ( = add 48 )  ( = add 64 ) ( = add 80 ) ( = add 96 ) ( = add 112 )) #f ]
      [ ( and ( >= add 14 ) ( equal? ( string-ref Posicion ( - add 14 ) ) #\- ) )  ( Revisar_salto_alfil_Negro_arriba_derecha ( - add 14 ) ) ]
      [ ( and ( >= add 14 ) ( or ( equal? ( string-ref Posicion ( - add 14 ) ) #\R ) ( equal? ( string-ref Posicion ( - add 14 ) ) #\A )) ( equal? ( string-ref Posicion ( - add 13 ) ) #\2 ) ) #t ]
      ))
( define ( Revisar_salto_alfil_Negro_arriba_izquierda add  )
   ( cond
      ;Rey-mira-arriba
      
      [ ( or ( = add 14 ) ( = add 30 ) ( = add 46 ) ( = add 62 )  ( = add 78 ) ( = add 94 ) ( = add 110 ) ( = add 126 )) #f ]
      [ ( and  ( >= add 18 ) ( equal? ( string-ref Posicion ( - add 18 ) ) #\- ) )  ( Revisar_salto_alfil_Negro_arriba_izquierda ( - add 18 ) ) ]
      [ ( and   ( >= add 18 ) ( or ( equal? ( string-ref Posicion ( - add 18 ) ) #\R ) ( equal? ( string-ref Posicion ( - add 18 ) ) #\A )) ( equal? ( string-ref Posicion ( - add 17 ) ) #\2 ) ) #t ]
      ))
;----------------------------------------------JAQUE-MATE----------------------------------------------------------------------
( define ( Cambiar_posicion_salir_jaque-simulacion x y add_Viejo add_Nuevo )
   ( string-set! remplazado 0 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 16)) )
   ( string-set! remplazado 1 ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 15)) )
   ( string-set! Posicion add_Nuevo ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 ) ) 16)) )
   ( string-set! Posicion ( + 1 add_Nuevo ) ( string-ref Posicion ( - ( char->integer ( string-ref add_viejo 0 ) ) 15)) )
   ( string-set! Posicion add_Viejo #\- )
   ( string-set! Posicion ( + 1 add_Viejo) #\- )
   ( if ( equal? #\K ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 16)) )
        (if ( equal? turno "1" )
            ( string-set! add-rey-blanco 0 ( integer->char add_Nuevo ) ) ( string-set! add-rey-negro 0 ( integer->char add_Nuevo ) )) (void) )  
   ( if ( and ( and ( equal? #f ( Revisar_casillas-blanco )) ( equal? #f ( Revisar_casillas-negro )) ))  
        (begin
          ( string-set! Posicion add_Viejo ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 16)) )
          ( string-set! Posicion ( + 1 add_Viejo ) ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 15)) )
          ( string-set! Posicion add_Nuevo ( string-ref remplazado 0 ) )
          ( string-set! Posicion ( + 1 add_Nuevo) ( string-ref remplazado 1 ) ) 
          #t
          )
        ( begin       
           ( string-set! Posicion add_Viejo ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 16)) )
           ( string-set! Posicion ( + 1 add_Viejo ) ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 15)) )
           ( string-set! Posicion add_Nuevo ( string-ref remplazado 0 ) )
           ( string-set! Posicion ( + 1 add_Nuevo) ( string-ref remplazado 1 ) )
           ( if ( equal? #\K ( string-ref Posicion ( - ( char->integer ( string-ref add_nuevo 0 ) ) 16)) )
                (if ( equal? turno "1" )
                    ( string-set! add-rey-blanco 0 ( integer->char add_Viejo ) ) ( string-set! add-rey-negro 0 ( integer->char add_Viejo ) )) (void) )
           )
        )
   )
( define ( Verificar1 add x y add1 x1 y1 )
   ( cond
      [ ( equal? ( string-ref Posicion add ) #\T ) ( if ( and ( equal? #t ( Mover_Torre x y ) ) ( equal? ( Evitar_Salto x y  x1 y1 add ) #t )) #t (void) )  ]
      [ ( equal? ( string-ref Posicion add ) #\C ) ( if ( equal? #t ( Mover_Caballo x y ) ) #t (void) )]
      [ ( equal? ( string-ref Posicion add ) #\A ) ( if ( and ( equal? #t ( Mover_Alfil x y ) ) ( equal? ( Evitar_Salto x y  x1 y1 add ) #t )) #t (void) ) ]
      [ ( equal? ( string-ref Posicion add ) #\R ) ( if ( and ( equal? #t  ( Mover_Reina x y ) ) ( equal? ( Evitar_Salto x y  x1 y1 add ) #t )) #t (void) ) ]
      [ ( equal? ( string-ref Posicion add ) #\K ) ( if ( and ( equal? #t ( Mover_Rey x y ) ) ( equal? ( Evitar_Salto x y  x1 y1 add ) #t )) #t (void) ) ]
      [ { or ( equal? ( string-ref Posicion add ) #\B ) ( equal? ( string-ref Posicion add ) #\N )}
        ( if ( and ( equal? #t ( Mover_Peon add x y ) ) ( equal? ( Evitar_Salto x y  x1 y1 add ) #t )) #t (void) ) ]
      [ else (void) ]
      );Final del cond
   )
( define (Jaque-mate pos x y pos1 x1 y1)
   ( string-set! add_viejo 0 ( integer->char ( + 16 pos) ) )
   ( string-set! add_nuevo 0 ( integer->char ( + 16 pos1) ) )
   ( string-set! x3 0 ( integer->char x1 ) )
   ( string-set! y3 0 ( integer->char y1 ) )
   (cond
     ( (> pos 127) #t)
     ( ( = x1 9 ) (Jaque-mate pos x y pos1 1 ( + y1 1 )  ) )
     ( (< pos1 127) ( if  (and ( equal? #t ( Verificar1 pos x y pos1 x1 y1 )) ( equal? #t ( Cambiar_posicion_salir_jaque-simulacion x y pos pos1 )))
                          #f ;si se cumple
                          (Jaque-mate  pos x y (+ pos1 2) ( + 1 x1 ) y1 ) ) );de lo contrario
     ( ( = x 9 ) (Jaque-mate pos 1 ( + y 1 ) pos1 x1 y1 ) )
     (else (Jaque-mate ( + pos 2) ( + 1 x ) y 0 1 1 )))
   )
   (define ( mostrar-jaque )
     ( if ( or ( equal? #t ( Revisar_casillas-blanco )) ( equal? #t ( Revisar_casillas-negro )) )
        ( ( ( draw-pixmap-posn "si_hay_jaque.png" ) Pantalla_De_Juego ) ( make-posn 750 150 ) )
        ( ( ( draw-pixmap-posn "no_hay_jaque.png" ) Pantalla_De_Juego ) ( make-posn 750 150 ) )
        ))
;-------------------------------------IMAGENES-DE-LAS-FICHAS-------------------------------------------------------------------
( define ( Imprimir_fichas contx )
   ( ( ( draw-pixmap-posn "Turno1.png" ) Pantalla_De_Juego ) ( make-posn 680 25 ) )
   ( cond
      [ ( <= contx 9 ) (((draw-pixmap-posn ( substring Imagenes ( + ( - contx 1 ) ( * 5 contx )) ( + 5 contx ( * contx 5 ) ) )) Pantalla_De_Juego)
                        (make-posn ( * 75 ( - contx 1 ) ) 55 ))  ( Imprimir_fichas ( + 1 contx ) ) ]
      [ ( < contx 18 ) (((draw-pixmap-posn ( substring Imagenes ( + ( - contx 1 ) ( * 5 contx )) ( + 5 contx ( * contx 5 ) ) )) Pantalla_De_Juego)
                        (make-posn ( - ( * 75 ( - contx 1 ) ) 605 ) 130 ))  ( Imprimir_fichas ( + 1 contx ) ) ]
      [ ( <= contx 25 ) (((draw-pixmap-posn ( substring Imagenes ( + ( - contx 1 ) ( * 5 contx )) ( + 5 contx ( * contx 5 ) ) )) Pantalla_De_Juego)
                         (make-posn ( - ( * 75 ( - contx 1 ) ) 1205 ) 505 ))  ( Imprimir_fichas ( + 1 contx ) ) ]
      [ ( <= contx 33 ) (((draw-pixmap-posn ( substring Imagenes ( + ( - contx 1 ) ( * 5 contx )) ( + 5 contx ( * contx 5 ) ) )) Pantalla_De_Juego)
                         (make-posn ( - ( * 75 ( - contx 1 ) ) 1800 ) 580 ))  ( Imprimir_fichas ( + 1 contx ) ) ]
      );Final del cond.
   ( ( ( draw-pixmap-posn "terminar.png" ) Pantalla_De_Juego ) ( make-posn 725 570 ) )
   );Final de la funcion Imprimir_fichas
( Imprimir_fichas 2 ) ( Hacer_click1 )
   )
( Abrir-pantalla-de-juego )