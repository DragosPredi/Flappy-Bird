#lang racket/gui
;Ignorați următoarele linii de cod. Conțin import-uri și export-uri necesare checker-ului.

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "random.rkt")
(require "abilities.rkt")
(require "constants.rkt")
;---------------------------------------checker_exports------------------------------------------------
(provide next-state)
(provide next-state-bird)
(provide next-state-bird-onspace)
(provide change)

(provide get-pipes)
(provide get-pipe-x)
(provide next-state-pipes)
(provide add-more-pipes)
(provide clean-pipes)
(provide move-pipes)

(provide invalid-state?)
(provide check-ground-collision)
(provide check-pipe-collisions)

(provide draw-frame)

(provide get-initial-state)
(provide get-bird)
(provide get-bird-y)
(provide get-bird-v-y)

; pipe
(provide get-pipes)
(provide get-pipe-x)

; score25
(provide get-score)

(provide get-abilities)
(provide get-abilities-visible)
(provide get-abilities-active)
; variables
(provide get-variables)
(provide get-variables-gravity)
(provide get-variables-momentum)
(provide get-variables-scroll-speed)

;---------------------------------------checker_exports------------------------------------------------
; Checker-ul contine un numar de teste, fiecare cu numele sau. In acest fisier veti gasi comentarii
; care incep cu TODO %nume_test, unde trebuie sa modificati sau sa implementati o functie, pentru
; a trece testul %nume_test.
;
;Initial state
; Primul pas pe care trebuie sa il facem este sa cream starea initiala a jocului.
; Aceasta va fi salvata in (get-initial-state), si trebuie sa incapsuleze toate informatiile
; necesare jocului, si anume: informatii despre pasare, despre pipes si despre powerups.
; Recomandam ca in pasare, sa retineti, printre altele, informatii despre y-ul curent
; si viteza pe y
; Pe parcursul temei, in state, salvati coordonatele colturilor din stanga sus ale obiectelor.
; Aceasta va face mai usoara atat logica miscarii obiectelor, cat si testarea cerintelor.
; Toate coordonatele oferite in comentarii sau in fisierul constants.rkt, se refera la
; coltul din stanga sus ale obiectelor!
;Inițial state
; Primul pas pe care trebuie să îl facem este să creăm starea inițială a jocului.
; Aceasta va fi salvată în (get-initial-state), și trebuie să incapsuleze toate informațiile
; necesare jocului, și anume: informații despre pasăre, despre pipes și, pentru bonus,
; despre powerups și despre variabilele de mediu.
; Recomandăm ca în pasăre, să rețineți, printre altele, informații despre y-ul curent
; și viteză pe y.
; Pe parcursul temei, în state, salvați coordonatele colțurilor din stânga sus ale obiectelor.
; Aceasta va face mai ușoară atât logică mișcării obiectelor, cât și testarea cerințelor.
; Toate coordonatele oferite în comentarii sau în fișierul variables.rkt se referă la
; colțul din stânga sus ale obiectelor!

;TODO 1
; După ce definiți structurile lui (get-initial-state) și a păsării, introduceți în prima
; pe cea din urmă. Colțul din stânga sus a păsării se va află inițial la:
;    y = bird-inițial-y
; și x = bird-x.
; (get-initial-state) va fi o funcție care va returna starea inițială a jocului.

;TODO 8
; În starea jocului, trebuie să păstrăm informații despre pipes. Pe parcursul jocului,
; pipe-urile se vor schimba, unele vor fi șterse și vor fi adăugate altele.
; După ce definiți structura pentru pipe și pentru mulțimea de pipes din stare,
; adăugați primul pipe în starea jocului. Acesta se va află inițial în afară ecranului.
; Celelalte pipe-uri vor fi adăugate ulterior, poziționându-le după acest prim pipe.
; Atenție! Fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap.
; Colțul din stânga sus al gap-ului dintre componentele primului pipe se va afla inițial la:
;    y = (+ added-number (random random-threshold)), pentru a da un element de noroc jocului,
; și x = scene-width,
; pentru a-l forța să nu fie inițial pe ecran.
; Atenție! Recomandăm să păstrați în stare colțul din stânga sus al chenarului lipsa
; dintre cele 2 pipe-uri!

;TODO 16
; Vrem o modalitate de a păstra scorul jocului. După ce definiți structura
; acestuia, adăugați scorul inițial, adică 0, în starea inițială a jocului.
; Atenție get-initial-state trebuie sa fie o funcție
; și trebuie apelată în restul codului.
(struct birdStruct (pos v-y))
(struct pipeStruct (x gapPos gapHeight))
(struct stateStruct (birdStruct pipesList scoreValue variables_ abilities_))

(define (get-initial-state)
  (let* ((birdie (birdStruct (make-posn bird-x bird-initial-y) 0))
         (pipes (cons (pipeStruct scene-width (make-posn scene-width (+ added-number (random random-threshold))) pipe-self-gap) '()))
         (scoreValue 0)
         (variables_ (variablesStruct initial-gravity initial-momentum initial-scroll-speed))
         (abilities_ (cons null null))
         (state (stateStruct birdie pipes scoreValue variables_ abilities_)))
    state))

;TODO 2
; După aceasta, implementați un getter care extrage din structura voastră
; pasărea, și un al doilea getter care extrage din structura pasăre
; y-ul curent pe care se află această.
(define (get-bird state)
   (stateStruct-birdStruct state))

(define (get-bird-y bird)
 (posn-y (birdStruct-pos bird)))

(define (get-bird-x bird)
 (posn-y (birdStruct-pos bird)))
;TODO 3
; Trebuie să implementăm logică gravitației. next-state-bird va primi drept
; parametri o structură de tip pasăre, și gravitația(un număr real). Aceasta va adaugă
; pozitiei pe y a păsării viteza acesteia pe y, si va adaugă vitezei pe y a păsării,
; gravitația.
(define (next-state-bird bird gravity)
  (struct-copy birdStruct bird
     [pos (make-posn (get-bird-x bird) (+ (get-bird-y bird) (get-bird-v-y bird)))]
     [v-y (+ (get-bird-v-y bird) gravity)]))

;TODO 4
; După aceasta, implementati un getter care extrage din structura voastră
; viteza pe y a păsării.
(define (get-bird-v-y bird)
 (birdStruct-v-y bird))

;TODO 6
; Dorim să existe un mod prin care să imprimăm păsării un impuls.
; Definiți funcția next-state-bird-onspace care va primi drept parametri
; o structură de tip pasăre, momentum(un număr real), și va schimba viteza
; pe y a păsării cu -momentum.
(define (next-state-bird-onspace bird momentum)
    (struct-copy birdStruct bird [v-y (- momentum)]))

; Change
; Change va fi responsabil de input-ul de la tastatură al jocului.
;TODO 7
; Acesta va primi drept parametri o structură de tip stare, și tasta pe
; care am apăsat-o. Aceasta va imprimă păsării momentum-ul, apelând
; funcția next-state-bird-onspace. Pentru orice altă tasta, starea rămâne aceeași.
(define (change current-state pressed-key)
  (if (equal? pressed-key " ")
      (struct-copy stateStruct current-state
      [birdStruct (next-state-bird-onspace (get-bird current-state) initial-momentum)])
      (struct-copy stateStruct current-state
      [birdStruct (next-state-bird-onspace (get-bird current-state) 0)])))

;TODO 9
; După ce ați definit structurile pentru mulțimea de pipes și pentru un singur pipe,
; implementați getterul get-pipes, care va extrage din starea jocului mulțimea de pipes,
; sub formă de lista.
(define (get-pipes state)
  (stateStruct-pipesList state))

;TODO 10
; Implementați get-pipe-x ce va extrage dintr-o singură structura de tip pipe, x-ul acesteia.
(define(get-pipe-x pipe)
  (pipeStruct-x pipe))

(define (get-pipeGap-y pipe)
  (posn-y (pipeStruct-gapPos pipe)))

(define (get-pipeGap-x pipe)
  (posn-x (pipeStruct-gapPos pipe)))

;TODO 11
; Trebuie să implementăm logica prin care se mișcă pipes.
; Funcția move-pipes va primi drept parametri mulțimea pipe-urilor din stare
; și scroll-speed(un număr real). Aceasta va scădea din x-ul fiecărui pipe
; scroll-speed-ul dat.
 (define (move-pipes pipes scroll-speed)
  (map (λ (element)
        (struct-copy pipeStruct element
         [x (- (get-pipe-x element) scroll-speed)]
         [gapPos (make-posn (- (get-pipe-x element) scroll-speed) (get-pipeGap-y element))]
         ))
       pipes))

;TODO 12
; Vom implementa logica prin care pipe-urile vor fi șterse din stare. În momentul
; în care colțul din DREAPTA sus al unui pipe nu se mai află pe ecran, acesta trebuie
; șters.
; Funcția va primi drept parametru mulțimea pipe-urilor din stare.
;
; Hint: cunoaștem lățimea unui pipe, pipe-width
(define (outOfWindow pipe)
  (if (> (+ (get-pipe-x pipe) pipe-width) 0) #t #f))

(define (clean-pipes pipes)
  (filter outOfWindow pipes))


;TODO 13
; Vrem să avem un sursa continuă de pipe-uri.
; Implementati funcția add-more-pipes, care va primi drept parametru mulțimea pipe-urilor
; din stare și, dacă avem mai puțin de no-pipes pipe-uri, mai adăugăm una la mulțime,
; având x-ul egal cu pipe-width + pipe-gap + x-ul celui mai îndepărtat pipe, în raport
; cu pasărea.
(define (lastPipe pipes)
  (if (> (length pipes) 1) (lastPipe (cdr pipes)) (car pipes)))

(define (create-new-pipe pipes)
  (let ((x (+ pipe-width pipe-gap (get-pipe-x (lastPipe pipes)))))
    (pipeStruct x (make-posn x (+ added-number (random random-threshold))) pipe-self-gap)))

(define (add-more-pipes pipes)
  (if (< (length pipes) no-pipes)
      (append pipes (list (create-new-pipe pipes)))
      pipes))

;TODO 14
; Vrem ca toate funcțiile implementate anterior legate de pipes să fie apelate
; de către next-state-pipes.
; Aceasta va primi drept parametri mulțimea pipe-urilor și scroll-speed-ul,
; și va apela cele trei funcții implementate anterior, în această ordine:
; move-pipes, urmat de clean-pipes, urmat de add-more pipes.
(define (next-state-pipes pipes scroll-speed)
  (add-more-pipes (clean-pipes (move-pipes pipes scroll-speed))))

;TODO 17
; Creați un getter ce va extrage scorul din starea jocului.
(define (get-score state)
  (stateStruct-scoreValue state))
  

(define (next-state-score score)
  (+ score 0.1))


;TODO 19
; Vrem să creăm logica coliziunii cu pământul.
; Implementati check-ground-collision, care va primi drept parametru
; o structura de tip pasăre, și returnează true dacă aceasta are coliziune
; cu pământul.
;
; Hint: știm înălțimea păsării, bird-height, și y-ul pământului, ground-y.
; Coliziunea ar presupune ca un colț inferior al păsării să aibă y-ul
; mai mare sau egal cu cel al pământului.
(define (check-ground-collision bird)
 (if (> (+ (get-bird-y bird) bird-height) ground-y) #t #f))

; invalid-state?
; invalid-state? îi va spune lui big-bang dacă starea curentă mai este valida,
; sau nu. Aceasta va fi validă atât timp cât nu avem coliziuni cu pământul
; sau cu pipes.
; Aceasta va primi ca parametru starea jocului.

;TODO 20
; Vrem să integrăm verificarea coliziunii cu pământul în invalid-state?.

;TODO 22
; Odată creată logică coliziunilor dintre pasăre și pipes, vrem să integrăm
; funcția nou implementată în invalid-state?.
(define (invalid-state? state)
  (cond
    [(check-ground-collision (get-bird state)) #t]
    [(check-pipe-collisions (get-bird state) (get-pipes state)) #t]
    [else #f]))


;TODO 21
; Odată ce am creat pasărea, pipe-urile, scor-ul și coliziunea cu pământul,
; următorul pas este verificarea coliziunii dintre pasăre și pipes.
; Implementati funcția check-pipe-collisions care va primi drept parametri
; o structură de tip pasăre, mulțimea de pipes din stare, și va returna
; true dacă există coliziuni, și false în caz contrar. Reiterând,
; fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap. Pot există
; coliziuni doar între pasăre și cele două părți. Dacă pasărea se află în
; chenarul lipsă, nu există coliziune.
;
; Hint: Vă puteți folosi de check-collision-rectangle, care va primi drept parametri
; colțul din stânga sus și cel din dreapta jos ale celor două dreptunghiuri
; pe care vrem să verificăm coliziunea.
(define (check-birdie-upper bird elem)
  (let ((birdY (get-bird-y bird))
               (upX (get-pipe-x elem))
               (upY 0))
                  (check-collision-rectangles
                   (make-posn bird-x birdY)
                   (make-posn (+ bird-x bird-width) (+ birdY bird-height))
                   (make-posn upX upY)
                   (make-posn (+ upX pipe-width) (+ upY (get-pipeGap-y elem)))
                   )))

(define (check-birdie-downed bird elem)
  (let ((birdY (get-bird-y bird))
               (upX (get-pipe-x elem))
               (upY (+ (get-pipeGap-y elem) pipe-self-gap)))
                  (check-collision-rectangles
                   (make-posn bird-x birdY)
                   (make-posn (+ bird-x bird-width) (+ birdY bird-height))
                   (make-posn upX upY)
                   (make-posn (+ upX pipe-width) (+ upY pipe-height))
                   )))
  
(define (check-pipe-collisions bird pipes)
  (cond
    [(empty? pipes) #f]
    [(check-birdie-upper bird (car pipes)) #t]
    [(check-birdie-downed bird (car pipes)) #t]
    [else (check-pipe-collisions bird (cdr pipes))]))
                                     

(define (check-collision-rectangles A1 A2 B1 B2)
  (match-let ([(posn AX1 AY1) A1]
              [(posn AX2 AY2) A2]
              [(posn BX1 BY1) B1]
              [(posn BX2 BY2) B2])
    (and (< AX1 BX2) (> AX2 BX1) (< AY1 BY2) (> AY2 BY1))))

;Next-state
; Next-state va fi apelat de big-bang la fiecare cadru, pentru a crea efectul de
; animație. Acesta va primi ca parametru o structură de tip stare, și va întoarce
; starea corespunzătoare următorului cadru.

;TODO 5
; Trebuie să integrăm funcția implementată anterior, și anume next-state-bird,
; în next-state.

;TODO 15
; Vrem să implementăm logică legată de mișcarea, ștergerea și adăugarea pipe-urilor
; în next-state. Acesta va apela next-state-pipes pe pipe-urile din starea curentă.

;TODO 18
; Vrem ca next-state să incrementeze scorul cu 0.1 la fiecare cadru.
(define (next-state state)
  (struct-copy stateStruct state
      [birdStruct (next-state-bird (get-bird state) initial-gravity)]
      [pipesList (next-state-pipes (get-pipes state) initial-scroll-speed)]
      [scoreValue (next-state-score (get-score state))]
      [abilities_ (next-abilities (get-abilities state) (get-bird state) initial-scroll-speed)]
      ))

; draw-frame
; draw-frame va fi apelat de big-bang dupa fiecare apel la next-state, pentru a afisa cadrul curent.
;TODO 23
; Fiecare cadru va fi desenat in urmatorul mod:
; bird peste ground, peste scor, peste pipes, peste empty-scene.
;
; Hint: score-to-image primeste un numar real si intoarce scor-ul sub forma de imagine;
; Scor-ul îl puteți plasa direct la coordonatele date, fără a mai face translatiile menționate mai jos.
; Noi tinem minte coltul din stanga sus al imaginii, insa, la suprapunerea unei imagini A peste o alta imagine,
; coordonatele unde plasam imaginea A reprezinta centrul acesteia. Trebuie facute translatiile de la coltul din stanga
; sus la centrul imaginilor.
; Variabile folosite in aceasta functie:
; bird -> bird-width si bird-height
; ground -> ground-y si ground-height, acesta va acoperi intreaga latime a ecranului
; scor -> text-x si text-y
; pipes -> pipe-width si pipe-height
(define bird-image (rectangle bird-width bird-height  "solid" "yellow"))
(define ground-image (rectangle scene-width ground-height "solid" "brown"))
(define initial-scene (rectangle scene-width scene-height "solid" "white"))

(define pipe-image (rectangle pipe-width pipe-height "solid" "green"))
(define pipe-gap-image (rectangle pipe-width pipe-self-gap "solid" "white"))

(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))
(define (score-to-image x)
(if SHOW_SCORE
	(apply text/font (~v (round x)) 24 "indigo" text-family)
	empty-image))

(define (draw-frame state)
  (place-visible-abilities (get-abilities state)
  (place-image bird-image (+ bird-x (quotient bird-width 2)) (+ (get-bird-y (get-bird state)) (quotient bird-height 2))
  (place-image ground-image (quotient scene-width 2) (+ ground-y (quotient ground-height 2))
  (place-image (score-to-image (get-score state)) text-x text-y
  (place-pipes (get-pipes state) initial-scene))))))
  
  
; Folosind `place-image/place-images` va poziționa pipe-urile pe scenă.
;---------------------------------------------------------------------
;This was my initial attempt to placing the pipes on the scene
;I keep it here to remind me how ugly stupidity is

;(define (get-upPipes-cords pipes)
;   (reverse (get-rev-upPipes-coords pipes '())))

;(define (get-rev-upPipes-coords pipes coords)
;  (if (empty? pipes) coords
;      (let ((pipeCord (pipeStruct-x (car pipes)))
;            (gap-cord-y (posn-y (pipeStruct-gapPos (car pipes)))))
;      (get-rev-upPipes-coords (cdr pipes) (cons (make-posn (+ pipeCord (quotient pipe-width 2)) (- gap-cord-y (quotient pipe-height 2))) coords)))))
;
;(define (get-pipes-images pipes)
; (foldl (λ (elem acc) (cons pipe-image acc)) '() pipes));
;
;(define (get-downPipes-cords pipes)
;   (reverse (get-rev-downPipes-coords pipes '())))
;
;(define (get-rev-downPipes-coords pipes coords)
;  (if (empty? pipes) coords
;      (let ((pipeCord (pipeStruct-x (car pipes)))
;            (gap-cord-y (posn-y (pipeStruct-gapPos (car pipes)))))
;      (get-rev-downPipes-coords (cdr pipes) (cons (make-posn (+ pipeCord (quotient pipe-width 2)) (+ (+ gap-cord-y pipe-self-gap) (quotient pipe-height 2))) coords)))))
  
;(define (place-pipes pipes scene)
;  (place-images (get-pipes-images pipes) (get-downPipes-cords pipes) (place-images (get-pipes-images pipes) (get-upPipes-cords pipes) scene)))
;---------------------------------------------------------------------------------------
(define (get-pipes-images pipes)
 (foldl (λ (elem acc) (cons pipe-image acc)) '() pipes));

(define (get-upCoords elem)
  (let ((x (pipeStruct-x elem)) (y (posn-y (pipeStruct-gapPos elem))))
    (make-posn (+ x (quotient pipe-width 2)) (- y (quotient pipe-height 2)))))

(define (get-downCoords elem)
  (let ((x (pipeStruct-x elem)) (y (posn-y (pipeStruct-gapPos elem))))
    (make-posn (+ x (quotient pipe-width 2)) (+ (+ y pipe-self-gap) (quotient pipe-height 2)))))

(define (place-pipes pipes scene)
  (place-images (get-pipes-images pipes) (map get-downCoords pipes) (place-images (get-pipes-images pipes) (map get-upCoords pipes) scene)))


; Bonus
; Completați abilities.rkt mai întâi, aceste funcții căt, apoi legați
; această funcționalitate la jocul inițial.


; Abilitatea care va încetini timpul va dura 10 de secunde, va avea imaginea (hourglass "mediumseagreen")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = max(5, scroll-speed - 1)
(define slow-ability (abilityStruct (hourglass "mediumseagreen")
                                    10
                                    null 
                                    (λ (scroll-speed) (max 5 (sub1 scroll-speed)))))
; Abilitatea care va accelera timpul va dura 30 de secunde, va avea imaginea (hourglass "tomato")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = scroll-speed + 1
(define fast-ability (abilityStruct (hourglass "tomato")
                                    30
                                    null
                                    (λ (scroll-speed) (add1 scroll-speed))))

; lista cu toate abilităţile posibile în joc
(define ABILITIES (list fast-ability slow-ability))

(struct variablesStruct (gravity momentum scroll-speed))


(define (get-variables state) (stateStruct-variables_ state))
(define (get-variables-gravity variables) (variablesStruct-gravity variables))
(define (get-variables-momentum variables) (variablesStruct-momentum variables))
(define (get-variables-scroll-speed variables) (variablesStruct-scroll-speed variables))

; Întoarce abilităţile din stare, cu o reprezentare
; intermediară care trebuie să conțină două liste:
;  - lista abilităţilor vizibile (încarcate în scenă dar nu neaparat vizibile pe ecran).
;  - lista abilităţilor activate (cu care pasărea a avut o coloziune).
(define (get-abilities x) (stateStruct-abilities_ x))

; Întoarce abilităţile vizibile din reprezentarea intermediară.
(define (get-abilities-visible x) (car x))

; Întoarce abilităţile active din reprezentarea intermediară.
(define (get-abilities-active x) (cdr x))

; Șterge din reprezentarea abilităţilor vizibile pe cele care nu mai sunt vizibile.
; echivalent cu clean-pipes.
(define (outOfWindowAb ability)
  (if (> (+ (posn-x (get-ability-pos ability)) 40) 0) #t #f))
            
(define (clean-abilities abilities)
	(filter outOfWindowAb abilities))


; Muta abilităţile vizibile spre stanga.
; echivalent cu move-pipes.
(define (move-abilities abilities scroll-speed)
	(map (λ (element)
               (struct-copy abilityStruct element
                 [pos (make-posn (- (posn-x (get-ability-pos element)) scroll-speed) (posn-y (get-ability-pos element)))]))
             abilities))


; Scurge timpul pentru abilităţile activate și le sterge pe cele care au expirat.
; Puteți să va folosiți de variabila globală fps.
(define (decrement-time element)
  (struct-copy abilityStruct element [time (- (get-ability-time element) (/ 1 fps))]))

(define (check-active element)
  (if (>= (get-ability-time element) 0) #t #f))

(define (time-counter abilities)
	(cons (car abilities) (filter check-active (map decrement-time (cdr abilities)))))

; Generează următoarele abilitați vizibile.
; *Atentie* La orice moment pe scena trebuie să fie exact DISPLAYED_ABILITIES
; abilităţi vizibile
; Folosiți funcția fill-abilities din abilities.rkt cât si cele scrise mai sus:
; move-abilities, clean-abilities, time-counter, etc..
(define (next-abilities-visible visible scroll-speed)
	(clean-abilities (position-abilities (fill-abilities (move-abilities visible scroll-speed) DISPLAYED_ABILITIES ABILITIES))))


; Generează structura intermediară cu abilități.
; Observați ca nu există next-abilities-active aceastea sunt acele abilităti
; întoarse next-abilities-visible care au o coliziune cu pasărea.
; Puteti folosi `filer`/`filter-not` ca sa verificați ce abilităti au și abilitați
; nu au coliziuni cu pasărea sau puteti folosi `partition`
(define (check-ability-collision bird ability )
  (let ((abX (posn-x (get-ability-pos ability)))
        (abY (posn-y (get-ability-pos ability)))
        (birdY (get-bird-y bird)))
    (check-collision-rectangles
     (make-posn bird-x birdY)
     (make-posn (+ bird-x bird-width) (+ birdY bird-height))
     (make-posn abX abY)
     (make-posn (+ abX 40) (+ abY 40)))))


         
(define (next-abilities abilities bird scroll-speed)
	 (let ((visibleAb (car abilities))
               (activeAb (cdr abilities)))
           (time-counter (cons (next-abilities-visible visibleAb scroll-speed) (filter (curry check-ability-collision bird) visibleAb)))))


           

; Dând-use variabilele actuale și abilitațile calculați care vor
; variabile finale folosite în joc
; Folositi compose-abilities
; Atenție când apelați `next-variables` în next-state dați ca paremetru
; initial-variables și nu variabilele aflate deja în stare
; In felul acesta atunci când
(define (next-variables variables abilities)
  'your-code-here)


; Folosind `place-image/place-images` va poziționa abilităţile vizibile la ability pos.
(define (place-visible-abilities abilities scene)
	(place-images (map get-ability-image (car abilities)) (map get-ability-pos (car abilities)) scene))

; Folosind `place-image/place-images` va poziționa abilităţile active
; în partea de sus a ecranului lângă scor.
; Imaginiile vor scalate cu un factor de 0.75 și așezate plecând
; de la ability-posn (constantă globală) cu spații de 50 de px.
; Imaginea cu indexul i va fi așezată la (ability-posn.x - 50*i, ability-posn.y)

(define (place-active-abilities abilities scene)
 '())

(module+ main
	(big-bang (get-initial-state)
	 [on-tick next-state (/ 1.0 fps)]
	 [to-draw draw-frame]
	 [on-key change]
	 [stop-when invalid-state?]
	 [close-on-stop #t]
	 [record? #f]))
