* Canabis - Canasta game for OS/2 and Windows
* uses FORTRAN/TK
* for Open Watcom FORTRAN/77
* by Robin Haberkorn
* Open Software License v.2.1

c$include fortrantk.fap

* Define dummy handlers, no user defined handlers necessary
c$include expat77.fap

c$ifdef __OS2__
c$define INCL_DOSNLS
c$include os2.fap
c$endif

      block data CANDAT
       include 'pile.fi'
       include 'msgcom.fi'

       data     drawn/.false./,
     &          blocked/.false./,
     &          initial/.true./,
     &          lrreached/.false./,
     &          lxpos/93/,
     &          lxposop/93/,
     &          cypos/360/,
     &          cyposop/310/,
     &          points/0,0/,
     &          limit/50,50/,            ! starting point limit = 50,
     &          canasta/-1,-1/           ! no canasta

       ! card image names:
       data     cards/'2c','2d','2h','2s','3c','3d','3h','3s','4c',
     &                '4d','4h','4s','5c','5d','5h','5s','6c','6d',
     &                '6h','6s','7c','7d','7h','7s','8c','8d','8h',
     &                '8s','9c','9d','9h','9s','ac','ad','ah','as',
     &                'b ','j ','jc','jd','jh','js','kc','kd','kh',
     &                'ks','qc','qd','qh','qs','tc','td','th','ts'/
      end

      program Canasta
       include       'fsublib.fi'
       include       'fortrantk.fi'
       include       'expat77.fi'
       include       'pile.fi'
       include       'msgcom.fi'
       logical       flag/.false./,
     &               taketalor/.false./   ! limit recently reached
       integer       x, n,
     &               dum,
     &               talorid
       character*256 str,
     &               nl*2
       integer       GetPoints,           ! function data types
     &               cardid
       logical       MoveWOT,
     &               GetTalor

       character*256 rootx, rooty,
     &               oldx,  oldy
       integer       borderx/0/, bordery/0/,
     &               scrollx/0/, scrolly/0/

       integer       config_parser,
     &               window, imageptr
       character*(*) buffer
       record /EXPAT77_node/ config_root

       character     decks*2(:,:), ids*20(:,:)

* Set codepage to 1004 under OS/2
* - because this does not seem to be possible under Windows -
c$ifdef __OS2__
       call DosSetProcessCp(1004)
c$endif

* Initialization
       if(TkInit('.'c)) then
        call MessageBeep(1000, 100)
        stop
       end if

       nl = char(13)//char(10)

       ! Allocate (global) decks/ids array
       allocate(decks(58, 108))         ! initially: 6+2*26 decks, 108 cards
       allocate(ids(58, 108))           ! initially: 4+2*26 decks (without
       decksptr = loc(decks)            ! pile and talor), 2 for Canastas
       idsptr   = loc(ids)              ! ->(pl/op)

       ! Open config file and write it to buffer -> unformatted
       open(10, file='canabis.xml', status='OLD', access='SEQUENTIAL',
     &      form='UNFORMATTED', recordtype='FIXED', action='READ',
     &      blocksize=256)
       allocate(buffer*filesize(10))
       read(10,iostat=dum) buffer
       close(10)

       ! Create tree
       config_parser = XML_ParserCreate(0)
       config_ptr = loc(config_root)
       call EXPAT77_PrepareTree(config_parser, config_ptr)
       call XML_Parse(config_parser, buffer, len(buffer), 1)
       deallocate(buffer)

       call InitDeck()
       call CreateWindow()

       loop
        cmpval = TkWait()

        ! Quit event/File->Exit
        ! -> Ask whether to exit
        if     (cmp('Quit'c)) then : event
         if(TkMessageBox('-message'c, msgstr(MESSAGE_CloseCanabis),
     &                   '-title'c,   'Canabis'c,
     &                   '-icon'c,    'question'c,
     &                   '-type'c,    'yesno'//dn) .EQ. 'yes'c)
     &    exit

        ! Options->Language->...
        else if(evname(cmpval) .EQ. 'options_lang'c) then
         call SetLanguage('msg\'//evarg(cmpval))
         ! Save new setting in tree
         call EXPAT77_SetChardata(
     &         EXPAT77_GetChild(
     &          EXPAT77_GetChild(
     &           EXPAT77_GetChild(config_ptr, 'canabis'c, 1),
     &          'visual'c, 1),
     &         'language'c, 1),
     &        1, evarg(cmpval))

        ! Options->Cards->...
        else if(evname(cmpval) .EQ. 'options_cards'c) then
         call SetCards('cards\'//evarg(cmpval))
         ! Save new setting in tree
         call EXPAT77_SetChardata(
     &         EXPAT77_GetChild(
     &          EXPAT77_GetChild(
     &           EXPAT77_GetChild(config_ptr, 'canabis'c, 1),
     &          'visual'c, 1),
     &         'cards'c, 1),
     &        1, evarg(cmpval))

        ! Options->Background->Color
        else if(cmp('options_bg_color'c)) then
         execute NoTransparency
         ! save setting to tree
         call EXPAT77_SetAttribute(
     &         EXPAT77_GetChild(
     &          EXPAT77_GetChild(
     &           EXPAT77_GetChild(config_ptr, 'canabis'c, 1),
     &          'visual'c, 1),
     &         'background'c, 1), 'type'c, '0'c)


        ! Options->Background->Image
        else if(cmp('options_bg_image'c)) then
         execute UpdateImage
         ! save setting to tree
         call EXPAT77_SetAttribute(
     &         EXPAT77_GetChild(
     &          EXPAT77_GetChild(
     &           EXPAT77_GetChild(config_ptr, 'canabis'c, 1),
     &          'visual'c, 1),
     &         'background'c, 1), 'type'c, '1'c)

        ! Options->Background->Pseudotransparency
        else if(cmp('options_bg_pseudo'c)) then
         ! call win_motion event for every window dragging/resizing
         call TkBind('.'c, '<Configure>'c, '*win_motion'//dn)

         ! we need to "hook" into the scroll event of the canvas
         ! because we need to move the bg_img if you scroll
         call TkConfig('.canvas'c,
     &         '-xscrollcommand'c, 'setRexxtk canvas_scrollx'c,
     &         '-yscrollcommand'c, 'setRexxtk canvas_scrolly'//dn)
         execute UpdateTransparency
         ! save setting to tree
         call EXPAT77_SetAttribute(
     &         EXPAT77_GetChild(
     &          EXPAT77_GetChild(
     &           EXPAT77_GetChild(config_ptr, 'canabis'c, 1),
     &          'visual'c, 1),
     &         'background'c, 1), 'type'c, '2'c)

        ! Options->Background->Choose color...
        else if(cmp('options_bg_chcol'c)) then
         ! get dialog title from menu label
         call TkConfig('.canvas'c, '-background'c,
     &    TkChooseColor('-title'c,
     &                  TkMenuEntryCget('.mainm.optionsm.bgm'c, '4'c,
     &                                  '-label'//dn),
     &                  '-initialcolor'c,
     &                  TkCget('.canvas'c, '-background'//dn)//dn)//dn)
         ! save new setting to tree
         call EXPAT77_SetChardata(
     &         EXPAT77_GetChild(
     &          EXPAT77_GetChild(
     &           EXPAT77_GetChild(
     &            EXPAT77_GetChild(config_ptr, 'canabis'c, 1),
     &           'visual'c, 1),
     &          'background'c, 1),
     &         'color'c, 1),
     &         1, TkCget('.canvas'c, '-background'//dn))

        ! Options->Background->Choose image...
        else if(cmp('options_bg_chimg'c)) then
         ! get dialog title from menu label
         str = TkGetOpenFile('-title'c,
     &                       TkMenuEntryCget('.mainm.optionsm.bgm'c,
     &                                       '5'c, '-label'//dn),
     &                       '-initialdir'c, '.\images'c,
     &                       '-defaultextension'c, '.gif'c,
     &                       '-filetypes'c, '{{GIF} {.gif}} '//
     &                       '{{PPM/PGM} {.ppm;.pgm}}'//dn)
c$ifdef __OS2__
         call ChangeDir('..'c) ! FileDialog changes working dir (just OS/2)
c$endif

         if(str(1:1) .NE. char(0)) then
          call TkImagePhoto('desk_bg'c, '-file'c, str//dn)
          call TkMenuEntryConfig('.mainm.optionsm.bgm'c, '1'c,
     &                           '-state'c, 'normal'//dn)
          call TkMenuEntryConfig('.mainm.optionsm.bgm'c, '2'c,
     &                           '-state'c, 'normal'//dn)

          if(TkVar('options_bg'//dn) .EQ. '1'c) then
           execute UpdateImage
          else if(TkVar('options_bg'//dn) .EQ. '2'c) then
           oldx = oldy = ' '                    ! update anyway
           execute UpdateTransparency
          end if

          ! Update setting in tree
          imageptr = EXPAT77_GetChild(
     &                EXPAT77_GetChild(
     &                 EXPAT77_GetChild(
     &                  EXPAT77_GetChild(config_ptr, 'canabis'c, 1),
     &                 'visual'c, 1),
     &                'background'c, 1),
     &               'image'c, 1)
          if(EXPAT77_GetChardata(imageptr, 1)) then   ! eventually create new
           call EXPAT77_SetChardata(imageptr, 1, str) ! chrdata element
            else
           call EXPAT77_AddChardata(imageptr, 1, str)
          end if
         end if

        ! Help->About...
        else if(cmp('help_about'c)) then
         call TkMessageBox('-message'c, msgstr(MESSAGE_About),
     &                     '-title'c,   'Canabis'c,
     &                     '-icon'c,    'info'//dn)

        ! Click on pile
        ! -> Draw a card
        else if(cmp('pile_click'c)) then
         if(drawn) then                  ! one draw per turn
          call Msg(errstr(ERROR_AlreadyDrawn), .true.)
           else
          if(MoveWOT(pnew(5) + 1, 1, 3, 5))    ! draw one + Red-3's drawn
     &     call UpdatePlRed()
          pnew(5) = 0                          ! cards

          call UpdatePile()              ! update graphics
          call UpdatePlCards()
          call UpdatePoints()

          drawn = .true.
         end if

        ! Click on talor
        ! -> Try to take talor
        else if(cmp('talor_click'c)) then
         if(drawn) then                   ! Either take from pile or take
                                          ! talor -> already drawn a card
          call Msg(errstr(ERROR_AlreadyDrawn), .true.)

           ! If talor isn't blocked and no blocking card is on
           ! top (Black 3's or Canasta main cards)
           else if((.NOT. blocked) .AND.
     &             (cardid(decks(2, size(2))) .NE. 2) .AND.
     &             (size(cardid(decks(2, size(2))) + 19) .EQ. 0)) then

          dum = 0                         ! count talor-type cards at hand
          do x = 1, size(3)
           if(cardid(decks(3, x)) .EQ. cardid(decks(2, size(2))))
     &      dum = dum + 1
          end do
          if((dum .LT. 2) .AND.
     &       (size(cardid(decks(2, size(2))) + 6) .EQ. 0)) then
           call Msg(errstr(ERROR_CannotTakeTalor), .true.)
           quit : event                 ! error if not enough cards
          end if                        ! at hand or at the table

          n = cardid(decks(2, size(2))) + 6
          if(size(n) .EQ. 0) pnew(n) = pnew(n) + 1
          size(n) = size(n) + 1
          mc(n) = mc(n) + 1
          points(1) = points(1) + GetPoints(decks(2, size(2)))
          decks(n, size(n)) = decks(2, size(2)) ! add top card to GO pile
          size(2) = size(2) - 1

          if(size(n) .NE. 1) then         ! If pile was already at table
           if(GetTalor(3, 5, 1))          ! (point limit reached!)
     &      call UpdatePlRed()            ! - take talor!
            else
           taketalor = .true.
           talorid   = n
          end if

          call MoveCanasta(n, 1)          ! if canasta ready, move it

          call UpdateGO(n)                ! update graphics
          call UpdatePlCards()
          call UpdatePoints()
          call UpdateTalor()

          drawn = .true.

           else                           ! Error: Talor is blocked/frozen

          call Msg(errstr(ERROR_FrozenTalor), .true.)
         end if

        ! Left click on player card
        ! -> Go out
        else if(evname(cmpval) .EQ. 'plc_click'c) then
         if(drawn) then
          if(size(3) .NE. 1) then        ! If enough cards...
           dum = str2int(evarg(cmpval))
           n = cardid(decks(3, dum)) + 6 ! Get id for pile

           if((size(n) .EQ. 6) .AND.     ! must be enough maincards
     &        (mc(n) .LT. 3)) then       ! >= 4
            call Msg(errstr(ERROR_MaincardsMinimum), .true.)
            quit : event
           end if

           x = points(1)
           points(1) = points(1) + GetPoints(decks(3, dum))
           addpts(1) = addpts(1) + GetPoints(decks(3, dum))

           if((x .LT. limit(1)) .AND.     ! point limit reached this round
     &        (points(1) .GE. limit(1))) lrreached = .true.

           mc(n)   = mc(n) + 1
           pnew(n) = pnew(n) + 1
           size(n) = size(n) + 1          ! exchange card
           decks(n, size(n)) = decks(3, dum)
           call Compress(3, dum, 1)

           if(taketalor         .AND.        ! if you take the talor - move
     &       (mc(n)     .GE. 3) .AND.        ! it, if point limit reached
     &       (points(1) .GE. limit(1))) then ! and enough main cards
            if(GetTalor(3, 5, 1)) call UpdatePlRed()
            pnew(n) = 0                      ! freeze this pile to table
            taketalor = .false.
            call UpdateTalor()
           end if

           call MoveCanasta(n, 1)         ! if canasta complete,
                                          ! move to new position
           call UpdateGO(n)               ! update graphics
           call UpdatePlCards()
           call UpdatePoints()

             else                         ! Error: Too few cards

            call Msg(errstr(ERROR_CannotDiscard), .true.)
           end if

           else                           ! Error: Draw a card first

          call Msg(errstr(ERROR_DrawFirst), .true.)
         end if

        ! Right click on player card
        ! -> Discard card / End turn
        ! [END GAME - no cards at hand]
        else if(evname(cmpval) .EQ. 'plc_clickr'c) then
         if(drawn) then

          flag = .false.
          do x=7, 19 ! 6+13
           if(pnew(x)) then
            if(flag) then                ! if point limit < 120, not
                                         ! reached/recently reached,
             call Msg(errstr(ERROR_JustOneCanasta), .true.)
             quit : event                ! more than one canasta -> error
            end if
            if(mc(x) .LT. 3) then        ! if not enough (3) main cards
                                         ! in the new GO piles -> error
             call Msg(errstr(ERROR_CardsMinimum), .true.)
             quit : event
            end if
            if((limit(1) .LT. 120) .AND.
     &        ((points(1) .LT. limit(1)) .OR.
     &          lrreached)) flag = .true.
           end if
          end do

          if((points(1) .NE. 0) .AND.        ! test whether you have enough
     &       (points(1) .LT. limit(1))) then ! points
           str = int2str(limit(1))
           call Msg('You need at least '//str(:ntlen(str))//
     &              ' points to go out.'c, .true.)
           quit : event
          end if

          lrreached = .false.

          do x=7, 19 ! 6+13              ! lay all cards out, except
           pnew(x) = 0                   ! Red-3's (pnew is for new
          end do                         ! Red-3's)

          dum = str2int(evarg(cmpval))
          addpts(1) = addpts(1) + GetPoints(decks(3, dum))
          if(cardid(decks(3, dum)) .EQ. 1)  ! block talor if you withdraw
     &     blocked = .true.                 ! a wildcard
          size(2) = size(2) + 1
          decks(2, size(2)) = decks(3, dum) ! exchange card
          call Compress(3, dum, 1)          ! compress pile

          call UpdateTalor()              ! update graphics
          call UpdatePlCards()
          call UpdatePoints()

          if(size(3) .EQ. 0) then         ! Won game?
           addpts(1) = addpts(1) + 100
           call TkMessageBox('-message'c, msgstr(MESSAGE_WonGame),
     &                       '-title'c,   'Canabis'c,
     &                       '-icon'c,    'info'//dn)
            else
           call EndTurn()                 ! Computers turn
          end if

           else                           ! Error: Draw a card first

          call Msg(errstr(ERROR_DrawFirst), .true.)
         end if

        ! Left click on GO pile
        ! -> request a wildcard
        else if(evname(cmpval) .EQ. 'go_click'c) then
         if(drawn) then
          if(cardid(decks(3, 1)) .EQ. 1) then ! If there is a wildcard
           if(size(3) .NE. 1) then            ! and it isn't the last card

            dum = str2int(evarg(cmpval))
            if((size(dum) .EQ. 6) .AND.       ! must be enough maincards
     &         (mc(dum) .LT. 4)) then         ! >= 4
             call Msg(errstr(ERROR_MaincardsMinimum), .true.)
             quit : event
            end if

            points(1) = points(1) + GetPoints(decks(3, 1))
            addpts(1) = addpts(1) + GetPoints(decks(3, 1))
            if(points(1) .GE. limit(1))   ! point limit reached this
     &       lrreached = .true.           ! round
            if(dum .EQ. 7) mc(dum) = mc(dum) + 1 ! Inc MC if wildcard add
            pnew(dum) = pnew(dum) + 1            ! to wildcard pile
            size(dum) = size(dum) + 1
            decks(dum, size(dum)) = decks(3, 1)  ! Exchange wildcard
            call Compress(3, 1, 1)

           if(taketalor         .AND.        ! if you take the talor - move
     &       (mc(dum)   .GE. 3) .AND.        ! it, if point limit reached
     &       (points(1) .GE. limit(1))) then ! and enough main cards
             if(GetTalor(3, 5, 1)) call UpdatePlRed()
             pnew(dum) = 0                   ! freeze this pile to table
             taketalor = .false.
             call UpdateTalor()
            end if

            call MoveCanasta(dum, 1)      ! if canasta complete,
                                          ! move to new position
            call UpdateGO(dum)            ! Update graphics
            call UpdatePlCards()
            call UpdatePoints()

             else                         ! Error: not enough cards

            call Msg(errstr(ERROR_CannotDiscard), .true.)
           end if

            else                          ! Error: no wildcard

           call Msg(errstr(ERROR_NoWildcard), .true.)
          end if

           else                           ! Error: draw card first

          call Msg(errstr(ERROR_DrawFirst), .true.)
         end if

        ! Right click on GO pile
        ! -> put card back
        else if(evname(cmpval) .EQ. 'go_clickr'c) then
         n = str2int(evarg(cmpval))
         if(drawn) then
          if(pnew(n)) then                ! Pile is new
           if(taketalor .AND.
     &        (n .EQ. talorid) .AND.      ! If went out by talor,
     &        (size(n) .EQ. 1)) then
            do x = 7, 19                  ! look for a second
             if((pnew(x) .NE. 0) .AND.    ! (new) pile
     &          (x .NE. n)) then          ! give error then
              call Msg(errstr(ERROR_RemoveOtherCards), .true.)
              quit : event
             end if
            end do
            size(2) = size(2) + 1         ! put card to talor
            decks(2, size(2)) = decks(n, size(n))
            call UpdateTalor()
            drawn = .false.               ! You can draw again
             else                         ! If went out by hand,
            size(3) = size(3) + 1         ! put card to hand
            decks(3, size(3)) = decks(n, size(n))
            addpts(1) = addpts(1) - GetPoints(decks(n, size(n)))
            call UpdatePlCards()
           end if

           if(cardid(decks(n, size(n))) .EQ.  ! if it's a main card -
     &        cardid(decks(n, 1)))            ! notice it :-)
     &      mc(n) = mc(n) - 1

           points(1) = points(1) - GetPoints(decks(n, size(n)))
           pnew(n) = pnew(n) - 1
           size(n) = size(n) - 1

           call UpdateGO(n)               ! update graphics
           call UpdatePoints()

            else                          ! Error: pile is old

           call Msg(errstr(ERROR_AlreadyAtTable), .true.)
          end if

           else                           ! Error: draw card first

          call Msg(errstr(ERROR_DrawFirst), .true.)
         end if

        ! Left click on canasta
        ! -> Add main card from hand
        else if(evname(cmpval) .EQ. 'can_click'c) then
         dum = str2int(evarg(cmpval))
         if(drawn) then
          x = 1                           ! Look for card at hand
          while(cardid(decks(3, x)) .NE. dum) do
           if(x .EQ. size(3)) then        ! If no card at hand to add
                                          ! output error and abort
            call Msg(errstr(ERROR_CannotAdd), .true.)
            quit : event
           end if
           x = x + 1
          end while

          if(size(3) .NE. 1) then         ! if this isn't the last card
           points(1) = points(1) + GetPoints(decks(3, x))
           addpts(1) = addpts(1) + GetPoints(decks(3, x))
           mc(dum+19) = mc(dum+19) + 1
           size(dum+19) = size(dum+19) + 1
           decks(dum+19, size(dum+19)) = decks(3, x) ! add card and
           call Compress(3, x, 1)                    ! remove it from hand

           call UpdatePlCards()           ! Update graphics
           call UpdateCanasta(dum)
           call UpdatePoints()

            else                          ! Erro: Too few cards

           call Msg(errstr(ERROR_CannotDiscard), .true.)
          end if

           else                           ! Error: draw card first

          call Msg(errstr(ERROR_DrawFirst), .true.)
         end if

        ! Hint events
        ! Display message string in status bar
        else if(evname(cmpval) .EQ. 'hint'c) then
         call Msg(msgstr(str2int(evarg(cmpval))), .false.)

        ! This event is called when pseudotransparency is on and
        ! window is moved/resized
        ! -> updates bg image if position changed
        ! -> includes cases when window is on a negative position
        else if(cmp('win_motion'c)) then
         execute UpdateTransparency

        ! Event is called when pseudotransparency is on
        ! called by the canvas to update y-direction scrollbar
        ! -> updates scrollbar and moves bg_img according scrollbar status
        ! -> so it is in the upper left corner of canvas
        else if(evname(cmpval) .EQ. 'canvas_scrolly'c) then
         str = evarg(cmpval)
         call TkSet('.yscroll'c, str(:index(str, ' '))//char(0),
     &                           str(index(str, ' '):)//dn)

         scrolly = str2real(str(:index(str, ' '))//char(0)) *
     &             str2int(TkCget('.canvas'c, '-height'//dn))
         execute CanvasCoords

        ! same as above just with the x-direction scrollbar
        else if(evname(cmpval) .EQ. 'canvas_scrollx'c) then
         str = evarg(cmpval)
         call TkSet('.xscroll'c, str(:index(str, ' '))//char(0),
     &                           str(index(str, ' '):)//dn)

         scrollx = str2real(str(:index(str, ' '))//char(0)) *
     &             str2int(TkCget('.canvas'c, '-width'//dn))
         execute CanvasCoords

        ! Unknown event
        else

         call Msg(errstr(ERROR_UnknownEvent), .true.)
        end if

       end loop

       ! Save window information back to tree:
       window = EXPAT77_GetChild(
     &           EXPAT77_GetChild(
     &            EXPAT77_GetChild(config_ptr, 'canabis'c, 1),
     &           'visual'c, 1),
     &          'window'c, 1)
       call EXPAT77_SetAttribute(window, 'left'c,
     &                           TkWinfo('x'c, '.'//dn))
       call EXPAT77_SetAttribute(window, 'top'c,
     &                           TkWinfo('y'c, '.'//dn))
       call EXPAT77_SetAttribute(window, 'width'c,
     &                           TkWinfo('width'c, '.'//dn))
       call EXPAT77_SetAttribute(window, 'height'c,
     &                           TkWinfo('height'c, '.'//dn))

       ! Save tree to file -> formatted open
       open(10, file='canabis.xml', status='OLD', access='SEQUENTIAL',
     &      recordtype='FIXED', action='WRITE', blocksize=256)
       call EXPAT77_TreeToXML(10, config_ptr)
       close(10)

       ! Free memory used by tree
       call EXPAT77_DisposeTree(config_ptr)
       call EXPAT77_ResetTree()
       call XML_ParserFree(config_parser)

       deallocate(decks, ids)

       call TkDestroy('.'//dn)            ! Unloading...
       if(TkUnload()) call MessageBeep(1000, 100)

       remote block CanvasCoords          ! update bg_img position
        call TkCanvasCoords('.canvas'c, 'bg_img'c,
     &                      int2str(borderx+scrollx),
     &                      int2str(bordery+scrolly)//dn)
       end block

       remote block NoTransparency         ! reset canvas events
        call TkBind('.'c, '<Configure>'c, dn)
        call TkConfig('.canvas'c,
     &                '-xscrollcommand'c, '.xscroll set'c,
     &                '-yscrollcommand'c, '.yscroll set'//dn)
        ! MAYBE WE NEED TO SAVE SCROLLBAR STATUS ANYWAY
        ! (IF YOU SWITCH BACK TO TRANSP. AND SCROLLBARS ARE USED)

        call TkTcl('canvas_bg blank'//dn)  ! blanks image
        oldx = oldy = ' '
        execute CanvasCoords
       end block

       remote block UpdateImage                  ! if you change (to) bg img
        execute NoTransparency
        ! MAYBE DECREASE/INCREASE DESK_BG SIZE TO FIT INTO CANVAS
        call TkTcl('canvas_bg copy desk_bg'//dn) ! assuming canvas_bg/desk_bg
                                                 ! already exist
       end block

       remote block UpdateTransparency     ! to update pseudotransp. bg img
        rootx = TkWinfo('rootx'c, '.canvas'//dn)
        rooty = TkWinfo('rooty'c, '.canvas'//dn)
        if(rootx .NE. oldx .OR. rooty .NE. oldy) then
         oldx = rootx
         oldy = rooty

         if(rootx(1:1) .EQ. '-') then
          borderx = -1*str2int(rootx)
          rootx   = '0'c
           else
          borderx = 0
         end if
         if(rooty(1:1) .EQ. '-') then
          bordery = -1*str2int(rooty)
          rooty   = '0'c
           else
          bordery = 0
         end if

         call TkTcl('canvas_bg copy desk_bg -from '//
     &              rootx(:ntlen(rootx))//' '//rooty//dn)
         execute CanvasCoords
        end if
       end block
      end

* Create graphical interface
      subroutine CreateWindow()
       implicit character*256 (A-Z)
       include   'fortrantk.fi'
       include   'expat77.fi'
       include   'pile.fi'
       include   'msgcom.fi'
       integer   tmpdll,
     &           x/0/
       integer   visual, window, chrptr
       character chrdum*(*)

       character decks*2(:,:), ids*20(:,:)

       allocate(decks(58, 108), location=decksptr)
       allocate(ids(58, 108), location=idsptr)

       visual = EXPAT77_GetChild(
     &           EXPAT77_GetChild(config_ptr, 'canabis'c, 1),
     &          'visual'c, 1)

       ! Menu:
       mainm = TkMenu('.mainm'//dn)
       gamem = TkMenu('.mainm.gamem'c,       ! "Game"
     &                '-tearoff'c, 'no'//dn)
       call TkAdd(gamem, 'command'c,         ! Add "Game->New Game"
     &            '-accelerator'c, 'Ctrl+N'c,
     &            '-rexx'c,        'game_new'//dn)
       call TkAdd(gamem, 'command'c,         ! Add "Game->Exit"
     &            '-accelerator'c, 'Ctrl+X'c,
     &            '-rexx'c,        'Quit'//dn)
       call TkAdd(mainm, 'cascade'c,
     &            '-menu'c, gamem//dn)

       optionsm = TkMenu('.mainm.optionsm'c,      ! "Options"
     &                   '-tearoff'c, 'no'//dn)
       langm = TkMenu('.mainm.optionsm.langm'c,   ! "Options->Language->"
     &                '-tearoff'c, 'no'//dn)
       cardsm = TkMenu('.mainm.optionsm.cardsm'c, ! "Options->Cards->"
     &                '-tearoff'c, 'no'//dn)
       bgm = TkMenu('.mainm.optionsm.bgm'c,       ! "Options->Background->"
     &              '-tearoff'c, 'no'//dn)
       call TkAdd(bgm, 'radiobutton'c,       ! Add "Options->Bg->Color"
     &            '-variable'c, 'options_bg'c,
     &            '-value'c,    '0'c,
     &            '-rexx'c,     'options_bg_color'//dn)
       call TkAdd(bgm, 'radiobutton'c,       ! Add "Options->Bg->Image"
     &            '-variable'c, 'options_bg'c,
     &            '-value'c,    '1'c,
     &            '-state'c,    'disabled'c,
     &            '-rexx'c,     'options_bg_image'//dn)
       call TkAdd(bgm, 'radiobutton'c,       ! Add "Options->Bg->Pseudotransp."
     &            '-variable'c, 'options_bg'c,
     &            '-value'c,    '2'c,
     &            '-state'c,    'disabled'c,
     &            '-rexx'c,     'options_bg_pseudo'//dn)
       call TkAdd(bgm, 'separator'//dn)      ! Add "Options->Bg->---"
       call TkAdd(bgm, 'command'c,           ! Add "Options->Bg->Ch. color..."
     &            '-rexx'c, 'options_bg_chcol'//dn)
       call TkAdd(bgm, 'command'c,           ! Add "Options->Bg->Ch. image..."
     &            '-rexx'c, 'options_bg_chimg'//dn)
       call TkAdd(bgm, 'command'c,           ! Add "Options->Bg->Gamma val..."
     &            '-rexx'c, 'options_bg_gamma'//dn)

       call TkAdd(optionsm, 'cascade'c,      ! Add "Options->Language"
     &            '-menu'c, langm//dn)
       call TkAdd(optionsm, 'cascade'c,      ! Add "Options->Cards"
     &            '-menu'c, cardsm//dn)
       call TkAdd(optionsm, 'separator'//dn) ! Add "Options->---"
       call TkAdd(optionsm, 'cascade'c,      ! Add "Options->Background"
     &            '-menu'c, bgm//dn)
       call TkAdd(mainm, 'cascade'c,
     &            '-menu'c, optionsm//dn)

       helpm = TkMenu('.mainm.helpm'c,       ! "Help"
     &                '-tearoff'c, 'no'//dn)
       call TkAdd(helpm, 'command'c,         ! Add "Help->Canasta rules..."
     &            '-rexx'c, 'help_rules'//dn)
       call TkAdd(helpm, 'command'c,         ! Add "Help->Playing Canabis..."
     &            '-accelerator'c, 'F1'c,
     &            '-rexx'c,        'help_canabis'//dn)
       call TkAdd(helpm, 'separator'//dn)    ! Add "Options->---"
       call TkAdd(helpm, 'command'c,         ! Add "Help->About..."
     &            '-rexx'c, 'help_about'//dn)
       call TkAdd(mainm, 'cascade'c,
     &            '-menu'c, helpm//dn)

       call TkConfig('.'c, '-menu'c, mainm//dn) ! attach main menu

       ! Add language and card packages to the menu
       ! choose package that's specified in the config file
       chrptr = EXPAT77_GetChardata(
     &           EXPAT77_GetChild(visual, 'language'c, 1), 1)
       allocate(chrdum*memlen(chrptr), location=chrptr)
       msgfile = chrdum

       file = GetFirstFile('msg\*.dll'c, FS_ALLFILES)
       loop
        tmpdll = LoadModule('msg\'//file)
        call TkAdd(langm, 'radiobutton'c,
     &             '-label'c,    GetResource(tmpdll, 300,
     &                                       MENU_LanguageName),
     &             '-variable'c, 'options_lang'c,
     &             '-value'c,    int2str(x),
     &             '-rexx'c,     'options_lang '//file//dn)
        call FreeModule(tmpdll)

        if(file(:len(chrdum)) .EQ. chrdum)
     &   call TkVar('options_lang'c, int2str(x)//dn) ! choose menu entry

        x = x + 1
        file = GetNextFile()
       until(file .EQ. char(0))
       call CloseSearch()

       chrptr = EXPAT77_GetChardata(
     &           EXPAT77_GetChild(visual, 'cards'c, 1), 1)
       allocate(chrdum*memlen(chrptr), location=chrptr)
       cardsfile = chrdum

       x = 0
       file = GetFirstFile('cards\*.dll'c, FS_ALLFILES)
       loop
        tmpdll = LoadModule('cards\'//file)
        call TkAdd(cardsm, 'radiobutton'c,
     &             '-label'c,    GetResource(tmpdll, 300,
     &                                       MENU_CardsName),
     &             '-variable'c, 'options_cards'c,
     &             '-value'c,    int2str(x),
     &             '-rexx'c,     'options_cards '//file//dn)
        call FreeModule(tmpdll)

        if(file(:len(chrdum)) .EQ. chrdum)
     &   call TkVar('options_cards'c, int2str(x)//dn) ! choose menu entry

        x = x + 1
        file = GetNextFile()
       until(file .EQ. char(0))
       call CloseSearch()

       ! Scrollbars
       yscroll = TkScrollbar('.yscroll'c,
     &                       '-orient'c,  'vertical'c,
     &                       '-command'c, '.canvas yview'//dn)
       xscroll = TkScrollbar('.xscroll'c,
     &                       '-orient'c,  'horizontal'c,
     &                       '-command'c, '.canvas xview'//dn)

       ! Canvas for drawing cards
       ! read background color from config tree
       chrptr =  EXPAT77_GetChardata(
     &            EXPAT77_GetChild(
     &             EXPAT77_GetChild(visual, 'background'c, 1),
     &            'color'c, 1), 1)
       allocate(chrdum*memlen(chrptr), location=chrptr)

       canvas = TkCanvas('.canvas'c,
     &                   '-background'c,     chrdum,
     &                   '-scrollregion'c,   '0 0 596 670'c,
     &                   '-width'c,          '596'c,
     &                   '-height'c,         '670'c,
     &                   '-xscrollcommand'c, '.xscroll set'c,
     &                   '-yscrollcommand'c, '.yscroll set'//dn)

       ! we need desk_bg/canvas_bg images for Image and Pseudotransparency
       ! background; read gamma from config tree
       call TkImagePhoto('canvas_bg'c, '-gamma'c, ! not adjustable via user
     &                   EXPAT77_GetAttribute(    ! interface yet
     &                    EXPAT77_GetChild(
     &                     EXPAT77_GetChild(visual, 'background'c, 1),
     &                    'image'c, 1),
     &                   'gamma'c)//dn)
       call TkCanvasImage(canvas, '0'c, '0'c,
     &                    '-image'c,  'canvas_bg'c,
     &                    '-tags'c,   'bg_img'c,
     &                    '-anchor'c, 'nw'//dn)

       ! Read image from config tree (to desk_bg) if there is one
       chrptr = EXPAT77_GetChardata(
     &           EXPAT77_GetChild(
     &            EXPAT77_GetChild(visual, 'background'c, 1),
     &           'image'c, 1), 1)
       if(chrptr) then
        allocate(chrdum*memlen(chrptr), location=chrptr)
        call TkImagePhoto('desk_bg'c,
     &                    '-file'c,  chrdum,
     &                    '-gamma'c, '1'//dn) ! enable image options:
        call TkMenuEntryConfig(bgm, '1'c, '-state'c, 'normal'//dn)
        call TkMenuEntryConfig(bgm, '2'c, '-state'c, 'normal'//dn)
         else
        call TkImagePhoto('desk_bg'//dn)
       end if

       ! now the menu options are eventually enabled ->
       call TkMenuInvoke(bgm,                ! read bg type from config
     &                   EXPAT77_GetAttribute(
     &                    EXPAT77_GetChild(visual, 'background'c, 1),
     &                   'type'c)//dn)       ! sets options_bg & calls event

       ! Rectangle item - just to assign right click event under non-OS/2
c$ifndef __OS2__
       call TkCanvasRectangle(canvas, '0'c, '0'c, '596'c, '670'c,
     &                        '-outline'c, char(0),
     &                        '-tags'c,    'bg_rect'//dn)
c$endif

       ! Status bar for messages and infos
       status = TkFrame('.status'//dn)
       call TkPack(TkLabel('.status.msg'c,
     &                     '-relief'c, 'sunken'c,
     &                     '-anchor'c, 'w'//dn),
     &             '-fill'c,   'x'c,
     &             '-expand'c, 'yes'c,
     &             '-side'c,   'left'//dn)
       call TkPack(TkLabel('.status.gopts'c,
     &                     '-relief'c, 'sunken'c,
     &                     '-width'c,  '10'c,
     &                     '-anchor'c, 'w'//dn),
     &             TkLabel('.status.allpts'c,
     &                     '-relief'c, 'sunken'c,
     &                     '-width'c,  '10'c,
     &                     '-anchor'c, 'w'//dn)//'A',
     &             '-side'c, 'left'//dn)

       ! Pack them
       call TkGrid(canvas, yscroll,
     &             '-row'c,    '0'c,
     &             '-sticky'c, 'news'//dn)
       call TkGrid(xscroll,
     &             '-row'c,    '1'c,
     &             '-sticky'c, 'news'//dn)
       call TkGrid(status, ' -'c,
     &             '-row'c,    '2'c,
     &             '-sticky'c, 'news'//dn)
       call TkGridRowConfig('.'c, '0'c,
     &                      '-weight'c, '1'//dn)
       call TkGridColumnConfig('.'c, '0'c,
     &                         '-weight'c, '1'//dn)

       ! Set language strings before window is diplayed and init cards
       ! before they are used
       call SetLanguage('msg\'//msgfile)
       call SetCards('cards\'//cardsfile)

       ! Draw field initially:
       ! Main pile
       call PlotTop('b ', 0, size(1), 1, 10, 223, .true.)

       ! Talor
       call PlotTop(decks(2, size(2)), 0, size(2), 3, 10, 350, .true.)

       call UpdateOpCards()              ! Players/Opponents cards
       call UpdatePlCards()              ! (Sets bindings, too)

       call UpdateOpRed()                ! Players/Opponents Red-3's
       call UpdatePlRed()

       call UpdatePoints()               ! Show points

       ! Initial Bindings... just left-clicks
       ! Pile:
       call TkCanvasBind(canvas, ids(57, 2),
     &                   '<Button-1>'c, '*pile_click'//dn)
       ! Talor:
       call TkCanvasBind(canvas, ids(57, 4),
     &                   '<Button-1>'c, '*talor_click'//dn)

       ! Hint bindings:
       call TkCanvasBind(canvas, ids(57, 2), '<Enter>'c,
     &                   '*hint '//int2str(MESSAGE_HelpPile)//dn)
       call TkCanvasBind(canvas, ids(57, 4), '<Enter>'c,
     &                   '*hint '//int2str(MESSAGE_HelpTalor)//dn)

       ! Right click popupmenus don't work under OS/2
c$ifndef __OS2__
       call TkCanvasBind(canvas, 'bg_rect'c, '<Button-3>'c,
     &                   'tk_popup .mainm.optionsm.bgm %X %Y'//dn)
c$endif

       ! Shotcuts:
       call TkBind('.'c, '<F1>'c, '*help_canabis'//dn)

       ! Don't destroy window if you press the close system button
       ! just do Quit event
       call TkWm('protocol'c, '.'c, 'WM_DELETE_WINDOW'c,
     &           'setRexxtk Quit'//dn)

       ! Try to read window's size & position from tree
       window = EXPAT77_GetChild(visual, 'window'c, 1)
       left   = EXPAT77_GetAttribute(window, 'left'c)
       top    = EXPAT77_GetAttribute(window, 'top'c)
       width  = EXPAT77_GetAttribute(window, 'width'c)
       height = EXPAT77_GetAttribute(window, 'height'c)
       if(left .NE. char(0))
     &  call TkWm('geometry'c, '.'c,
     &            '+'//left(:ntlen(left))//'+'//top//dn)

       call TkTcl('update'//dn)
       call TkWm('maxsize'c, '.'c,      ! window's maxsize to actual size
     &           TkWinfo('width'c, '.'//dn),
     &           TkWinfo('height'c, '.'//dn)//dn)

       if(width .NE. char(0))
     &  call TkWm('geometry'c, '.'c,
     &            width(:ntlen(width))//'x'//height//dn)
      end

* Initialize game
* (mix decks and give cards)
      subroutine InitDeck()
       implicit character*256 (A-Z)
       include  'fortrantk.fi'
       include  'pile.fi'
       include  'fsublib.fi'   ! URAND
       integer   x/0/, i,
     &           rnd
       integer*2 h, m, s, seed
       integer   cardid        ! function data types
       logical   MoveWOT

       character decks*2(:,:)

       allocate(decks(58, 108), location=decksptr)

       ! Initialize decks array, 108 cards - canasta set
       do i=1, 54
        if(cards(i) .EQ. 'b ') then
         decks(1, 2*i-1) = 'j '
         decks(1, 2*i)   = 'j '
          else
         decks(1, 2*i-1) = cards(i) ! cannot use double assignment,
         decks(1, 2*i)   = cards(i) ! for some frakkin reason
        end if
       end do

       ! "Give" cards
       ! Mix 1. deck (pile) (exchange every card with a random one)
       call gettim(h, m, s, seed)
       do x=1, 108
        rnd = urand(seed)*107 + 1
        dum = decks(1, x)
        decks(1, x)   = decks(1, rnd)
        decks(1, rnd) = dum
       end do

       ! Set 2. deck (talor) - at least 4 cards
       do x=0, 3
        decks(2, 4-x) = decks(1, 108-x)
       end do
       size(1) = 104
       size(2) = 4
       ! If last card is wildcard, Black-3 or Red-3,
       ! add other cards to the talor
       while((cardid(decks(2, size(2))) .EQ. 1) .OR.
     &       (cardid(decks(2, size(2))) .EQ. 2) .OR.
     &       (cardid(decks(2, size(2))) .EQ. 14)) do
        size(2) = size(2) + 1
        decks(2, size(2)) = decks(1, size(1))
        size(1) = size(1) - 1
       end while

       ! Set 3./4. deck (Players cards) - each 13 cards of 1. deck
       ! Find Red-3's
       call MoveWOT(13, 1, 3, 5)
       call MoveWOT(13, 1, 4, 6)
       call SortPile(4)
      end

* Set message/error and menu strings to new language
* msgfile = null terminated filename (message DLL)
      subroutine SetLanguage(msgfile)
       implicit  integer(A-Z)
       character msgfile*(*)
       include   'fortrantk.fi'
       include   'msgcom.fi'

       msgdll = LoadModule(msgfile)             ! open new message string DLL
       do x=1, 13                               ! initialize message strings
        errstr(x) = GetResource(msgdll, 400, x) ! from compiled resources
       end do
       do x=1, 9
        msgstr(x) = GetResource(msgdll, 500, x)
       end do

       ! Window title
c$ifdef     __OS2__
       call TkWm('title'c, '.'c,
     &           GetResource(msgdll, 600, MENU_Title_OS2)//dn)
c$elseifdef __WIN__
       call TkWm('title'c, '.'c,
     &           GetResource(msgdll, 600, MENU_Title_Windows)//dn)
c$endif

       ! Game, Game->New Game, Game->Exit
       call ChangeMenu('.mainm'c, 1, msgdll, MENU_Game)
       call ChangeMenu('.mainm.gamem'c, 0, msgdll, MENU_Game_NewGame)
       call ChangeMenu('.mainm.gamem'c, 1, msgdll, MENU_Game_Exit)

       ! Options, Options->Language, Options->Cards, Options->Background
       call ChangeMenu('.mainm'c, 2, msgdll, MENU_Options)
       call ChangeMenu('.mainm.optionsm'c, 0,
     &                 msgdll, MENU_Options_Language)
       call ChangeMenu('.mainm.optionsm'c, 1,
     &                 msgdll, MENU_Options_Cards)
       call ChangeMenu('.mainm.optionsm'c, 3,
     &                 msgdll, MENU_Options_Background)

       ! Bg->Color, Bg->Image, Bg->Pseudeotransp., Bg->Ch. color...,
       ! Bg->Ch. image..., Bg->Gamma value...
       call ChangeMenu('.mainm.optionsm.bgm'c, 0,
     &                 msgdll, MENU_Background_Color)
       call ChangeMenu('.mainm.optionsm.bgm'c, 1,
     &                 msgdll, MENU_Background_Image)
       call ChangeMenu('.mainm.optionsm.bgm'c, 2,
     &                 msgdll, MENU_Background_Pseudotransparency)
       call ChangeMenu('.mainm.optionsm.bgm'c, 4,
     &                 msgdll, MENU_Background_ChooseColor)
       call ChangeMenu('.mainm.optionsm.bgm'c, 5,
     &                 msgdll, MENU_Background_ChooseImage)
       call ChangeMenu('.mainm.optionsm.bgm'c, 6,
     &                 msgdll, MENU_Background_GammaValue)

       ! Help, Help->Canasta rules..., Help->Playing Canabis..., Help->About...
       call ChangeMenu('.mainm'c, 3, msgdll, MENU_Help)
       call ChangeMenu('.mainm.helpm'c, 0,
     &                 msgdll, MENU_Help_CanastaRules)
       call ChangeMenu('.mainm.helpm'c, 1,
     &                 msgdll, MENU_Help_PlayingCanabis)
       call ChangeMenu('.mainm.helpm'c, 3, msgdll, MENU_Help_About)

       ! Set status bar to welcome message
       call Msg(msgstr(MESSAGE_Welcome), .false.)

       call FreeModule(msgdll)
      end

* Helper function for SetLanguage
* changes Menu entry to a given resource, looks for '&' to underline
* a letter
* menu = nt string menu, n = index to change, resid = resource id of string
      subroutine ChangeMenu(menu, n, msgdll, resid)
       implicit  integer(A-Z)
       character menu*(*)
       include   'fortrantk.fi'
       character dum*256

       dum = GetResource(msgdll, 600, resid)
       pos = index(dum, '&')
       if(pos) dum = dum(:pos-1)//dum(pos+1:)
       call TkMenuEntryConfig(
     &       menu, int2str(n),
     &       '-label'c,     dum,
     &       '-underline'c, int2str(pos-1)//dn)
      end

* Set cards to new images from cards .DLL
* cardsfile = cards resource .DLL file
      subroutine SetCards(cardsfile)
       implicit  integer(A-Z)
       character cardsfile*(*)
       include   'fortrantk.fi'
       include   'pile.fi'
       character buf*(*), buf2*(*)
       record    /RXSTRING/ res

       cardsdll = LoadModule(cardsfile) ! load cards resource DLL
                               ! card images are saved as resources
       do x = 1, 54            ! read all card resources
        res = GetResourceEx(cardsdll, 256, x)
        allocate(buf*res.strlength, location=res.strptr)
        allocate(buf2*res.strlength+2)
        buf2 = buf              ! write resource in buffer
        buf2(len(buf2)-1:) = dn ! and add DN to the buffer
        call TkImagePhoto(cards(x)(:lentrim(cards(x)))//char(0),
     &                    '-data'c, buf2)

        deallocate(buf, buf2)
        call CloseResource(res.strptr)
       end do

       call FreeModule(cardsdll)
      end

* Move cards, look for Red-3's and evtl. update points
* (from top of source to top of destination pile)
* n = number of cards, s = source pile,
* d = destiny pile, r = Red-3's pile
* = .true. if Red-3 found
      logical function MoveWOT(n, s, d, r)
       integer n, s, d, r
       include 'pile.fi'
       integer x
       integer cardid,
     &         GetPoints

       character decks*2(:,:)

       allocate(decks(58, 108), location=decksptr)

       MoveWOT = .false.
       do x = 1, n
        while(cardid(decks(s, size(s))) .EQ. 14) do ! Red-3 found
         size(r) = size(r) + 1                      ! Put it to r
         decks(r, size(r)) = decks(s, size(s))
         size(s) = size(s) - 1
         MoveWOT = .true.
        end while

        if(d .EQ. 3) then
         addpts(1) = addpts(1) - GetPoints(decks(s, size(s)))
        else if(d .EQ. 4) then
         addpts(2) = addpts(2) - GetPoints(decks(s, size(s)))
        end if
        size(d) = size(d) + 1                       ! exchange card
        decks(d, size(d)) = decks(s, size(s))
        size(s) = size(s) - 1
       end do
      end

* Move Canasta n from table to the Canasta storage (n+13)
* update points for player p
      subroutine MoveCanasta(n, p)
       integer n, p
       include 'pile.fi'
       integer x
       integer cardid

       character decks*2(:,:)

       allocate(decks(58, 108), location=decksptr)

       if(size(n) .EQ. 7) then           ! move canasta if complete
        do x=1, 7
         decks(n+13, x) = decks(n, x)
        end do
        size(n+13) = 7
        mc(n+13)   = mc(n)

        if(cardid(decks(n, 1)) .EQ. 1) then ! give...
         addpts(p) = addpts(p) + 1000       ! 1000p for Joker,
        else if(mc(n) .EQ. 7) then
         addpts(p) = addpts(p) + 500        ! 500p for natural,
        else
         addpts(p) = addpts(p) + 300        ! 300p for mixed canasta
        end if
        canasta(p) = 1                      ! positive score

        size(n) = pnew(n) = mc(n) = 0
        lrreached = .false.              ! limit reached anyway!

        if(p .EQ. 1) then                ! draw canasta
         call UpdateCanasta(n-6)
          else
         call UpdateCanastaOp(n-32)
        end if
       end if
      end

* Move talor to hand (d), look for Red-3's and put them to r
* updates Red-3 graphics
* update points for player p
* .true. if Red-3 in talor
      logical function GetTalor(d, r, p)
       integer d, r, p
       include 'pile.fi'
       integer x
       integer cardid,
     &         GetPoints

       character decks*2(:,:)

       allocate(decks(58, 108), location=decksptr)

       GetTalor = .false.
       do x = 1, size(2)
        if(cardid(decks(2, x)) .EQ. 14) then
         size(r) = size(r) + 1
         pnew(r) = pnew(r) + 1
         decks(r, size(r)) = decks(2, x)
         GetTalor = .true.
          else
         addpts(p) = addpts(p) - GetPoints(decks(2, x))
         size(d) = size(d) + 1
         decks(d, size(d)) = decks(2, x)
        end if
       end do
       size(2) = 0
       initial = .false.
      end

* Sort pile n with BubbleSort
* (lexically)
* however put Jokers first :)
      subroutine SortPile(n)
       integer   n
       include   'pile.fi'
       logical   moved
       integer   x, y
       character dum*2

       character decks*2(:,:)

       allocate(decks(58, 108), location=decksptr)

       moved = .true.
       while(moved) do
        moved = .false.

        do x=1, size(n)-1
         if(lgt(decks(n, x), decks(n, x+1))) then
          dum = decks(n, x+1)
          decks(n, x+1) = decks(n, x)
          decks(n, x) = dum
          moved = .true.
         end if
        end do
       end while

       do x=1, size(n)
        if(decks(n, x) .EQ. 'j ') then     ! look for Jokers
         dum = decks(n, 1)
         decks(n, 1) = decks(n, x)         ! insert Joker

         do y=x, 3, -1                     ! make space to insert
          decks(n, y) = decks(n, y-1)      ! the first card again
         end do
         decks(n, 2) = dum                 ! ...insert it
        end if
       end do
      end

* Compress pile
* n = pile, p = position in pile, c = number of cards to move
      subroutine Compress(n, p, c)
       integer n, p, c
       include 'pile.fi'
       integer x

       character decks*2(:,:)

       allocate(decks(58, 108), location=decksptr)

       size(n) = size(n) - c
       do x=p, size(n)                   ! Compress pile
        decks(n, x) = decks(n, x+c)
       end do
      end

* Get card-type id
      integer function cardid(card)
       character card*2

       select(card)                      ! set right pile index
        case('2')                        ! according to the card
         cardid = 1
        case('3')                        ! just black 3's
         cardid = 2
        case('4')
         cardid = 3
        case('5')
         cardid = 4
        case('6')
         cardid = 5
        case('7')
         cardid = 6
        case('8')
         cardid = 7
        case('9')
         cardid = 8
        case('a')
         cardid = 9
        case('j')
         cardid = 10
        case('k')
         cardid = 11
        case('q')
         cardid = 12
        case('t')
         cardid = 13
       end select
       if(card .EQ. 'j ') cardid = 1
       if((card .EQ. '3d') .OR.          ! Red-3's
     &    (card .EQ. '3h')) cardid = 14
      end

* Get points for a given card
      integer function GetPoints(card)
       character*2 card

       select(card)
        case('2', 'a')                     ! 2 and As
         GetPoints = 20
        case('3', '4', '5', '6', '7')      ! Colors
         GetPoints = 5
        case('8', '9', 't', 'j', 'q', 'k') ! Higher colors
         GetPoints = 10
       end select
       if((card .EQ. '3d') .OR.            ! Red-3's
     &    (card .EQ. '3h')) GetPoints = 100
       if(card .EQ. 'j ') GetPoints = 50   ! Jokers
      end

* Display error/msg in the status bar
* err = error yes/no
      subroutine Msg(str, err)
       character*(*) str
       logical       err
       include       'fortrantk.fi'
       character*256 dum

       if(err) then
        call MessageBeep(1000, 100)
        dum = 'red'c
         else
        dum = 'black'c
       end if
       call TkConfig('.status.msg'c,
     &               '-text'c, str,
     &               '-fg'c,   dum//dn)
      end

* Update points display in the status bar
      subroutine UpdatePoints()
       include       'fortrantk.fi'
       include       'pile.fi'
       character*256 dum,
     &               dum2,
     &               str
       integer       dum3

       dum = int2str(points(1))
       if(points(1) .LT. limit(1)) then
        dum2 = int2str(limit(1))
        str = dum(:ntlen(dum))//
     &        ' ('//dum2(:ntlen(dum2))//')'//char(0)
         else                           ! Show just points if the
        str = dum                       ! limit is reached
       end if
       call TkConfig('.status.gopts'c,
     &               '-text'c, str//dn)

       dum3 = canasta(1)*(points(1)+size(5)*100)+addpts(1)
       if(dum3 .GE. 0) then
        dum = 'black'c
         else
        dum = 'red'c
       end if
       call TkConfig('.status.allpts'c,
     &               '-fg'c,   dum,
     &               '-text'c, int2str(dum3)//dn)
      end

* Show pile horizontally/verically
* eventually just backside
* eventualle in reverse order
* eventually save ids in IDS(id, ...)
      subroutine Plot(n, id, curx, cury, dir, back, rev)
       implicit  integer(A-Z)
       logical   dir,
     &           back,
     &           rev
       include   'fortrantk.fi'
       include   'pile.fi'
       character cdum*2,
     &           anchor*2,
     &           dum*20

       character decks*2(:,:), ids*20(:,:)

       allocate(decks(58, 108), location=decksptr)
       allocate(ids(58, 108), location=idsptr)

       if(size(n) .GE. 1) then
        if(back) cdum = 'b '
        if(rev) then
         i = -1
         if(dir) then
          anchor = 'sw'
           else
          anchor = 'ne'
         end if
          else
         i = 1
         anchor = 'nw'
        end if

        do x=1, size(n)
         if(.NOT. back) cdum = decks(n, x)
         dum = TkCanvasImage('.canvas'c, int2str(curx), int2str(cury),
     &                       '-image'c,  cdum(:lentrim(cdum))//char(0),
     &                       '-anchor'c, anchor//dn)
         if(id) then
          ids(id, x) = dum
          idsize(id) = size(n)
         end if
         if(dir) then
          cury = cury + 16*i
           else
          curx = curx + 13*i
         end if
        end do

         else

        idsize(id) = 0
       end if
      end

* Show top of pile
* and size of pile ("nat/count" -> right or just "count" -> above)
* top: anchor at top of card
      subroutine PlotTop(card, nat, count, id, curx, cury, top)
       character card*2
       integer   nat,
     &           count,
     &           id,
     &           curx,
     &           cury
       logical   top
       include   'fortrantk.fi'
       include   'pile.fi'
       character dum*256,
     &           anchor*2

       character ids*20(:,:)

       allocate(ids(58, 108), location=idsptr)

       ! Center number (count) / put it right to the card
       ! Saves 2 ids
       if(top) then
        anchor = 'nw'
         else
        anchor = 'sw'
       end if
       if(nat) then
        dum = int2str(nat)
        ids(57, id) =
     &   TkCanvasText('.canvas'c, int2str(curx+78), int2str(cury),
     &                '-text'c,   dum(:ntlen(dum))//'/'//
     &                            int2str(count),
     &                '-anchor'c, anchor//char(0),
     &                '-fill'c,   'white'//dn)
         else
        ids(57, id) =
     &   TkCanvasText('.canvas'c, int2str(curx+35), int2str(cury-20),
     &                '-text'c,   int2str(count),
     &                '-anchor'c, 'n'c,
     &                '-fill'c,   'white'//dn)
       end if

       ids(57, id+1) =
     &  TkCanvasImage('.canvas'c, int2str(curx), int2str(cury),
     &                '-image'c,  card(:lentrim(card))//char(0),
     &                '-anchor'c, anchor//dn)

       if(id+1 .GE. idsize(57))
     &  idsize(57) = idsize(57) + 2
      end

* Graphics update routines
* Update pile graphics (number)
      subroutine UpdatePile()
       include 'fortrantk.fi'
       include 'pile.fi'

       character ids*20(:,:)

       allocate(ids(58, 108), location=idsptr)

       call TkItemConfig('.canvas'c, ids(57, 1),
     &                   '-text'c,   int2str(size(1))//dn)
      end

* Update talor graphics with number
      subroutine UpdateTalor()
       include   'fortrantk.fi'
       include   'pile.fi'
       character cdum*2

       character decks*2(:,:), ids*20(:,:)

       allocate(decks(58, 108), location=decksptr)
       allocate(ids(58, 108), location=idsptr)

       call TkItemConfig('.canvas'c, ids(57, 3),
     &                   '-text'c,   int2str(size(2))//dn)
       if(size(2)) then
        if((size(2) .EQ. 2) .AND. initial) then
         cdum = 'b '                      ! If initial pile - show back
          else                            ! as 2. card
         cdum = decks(2, size(2))
        end if
         else
        cdum = ' '
       end if
       call TkItemConfig('.canvas'c, ids(57, 4),
     &                   '-image'c,  cdum(:lentrim(cdum))//dn)
      end

* Update player cards graphics
* (sort and redraw cards)
      subroutine UpdatePlCards()
       include 'fortrantk.fi'
       include 'pile.fi'
       include 'msgcom.fi'
       integer x

       character ids*20(:,:)

       allocate(ids(58, 108), location=idsptr)

       if(idsize(1)) then
        do x=1, idsize(1)
         call TkDelete('.canvas'c, ids(1, x)//dn)
        end do
       end if
       call SortPile(3)                         ! 193px for going out:
       call Plot(3, 1, 93, 563, .false., .false., .false.)

       do x=1, idsize(1)                 ! Set new event bindings
        ! Left click: go out
        call TkCanvasBind('.canvas'c, ids(1, x),
     &                    '<Button-1>'c, '*plc_click '//int2str(x)//dn)
        ! Right click: withdraw card
        call TkCanvasBind('.canvas'c, ids(1, x),
     &                    '<Button-3>'c, '*plc_clickr '//int2str(x)//dn)
        ! Mouse over: show hint
        ! COULD BE IMPROVED - JUST ONE EVENT
        call TkCanvasBind('.canvas'c, ids(1, x), '<Enter>'c,
     &                    '*hint '//int2str(MESSAGE_HelpHand)//dn)
       end do
      end

* Update opponent cards graphics
* (redraw cards)
      subroutine UpdateOpCards()
       include 'fortrantk.fi'
       include 'pile.fi'
       integer x

       character ids*20(:,:)

       allocate(ids(58, 108), location=idsptr)

       if(idsize(2)) then
        do x=1, idsize(2)
         call TkDelete('.canvas'c, ids(2, x)//dn)
        end do
       end if
       call Plot(4, 2, 503, 10, .false., .true., .true.) ! just backside
      end

* Update players Red-3's
* (redraw cards)
      subroutine UpdatePlRed()
       include 'fortrantk.fi'
       include 'pile.fi'
       integer x

       character ids*20(:,:)

       allocate(ids(58, 108), location=idsptr)

       if(idsize(3)) then
        do x=1, idsize(3)
         call TkDelete('.canvas'c, ids(3, x)//dn)
        end do
       end if
       call Plot(5, 3, 400, 360, .true., .false., .false.) ! vertically
      end

* Update opponent Red-3's
* (redraw cards)
      subroutine UpdateOpRed()
       include 'fortrantk.fi'
       include 'pile.fi'
       integer x

       character ids*20(:,:)

       allocate(ids(58, 108), location=idsptr)

       if(idsize(4)) then
        do x=1, idsize(4)
         call TkDelete('.canvas'c, ids(4, x)//dn)
        end do
       end if
       call Plot(6, 4, 400, 310, .true., .false., .true.) ! vertically,
      end                                                 ! reverse

* Update additional pile (meld) - redraw cards
* Player
      subroutine UpdateGO(n)
       integer   n
       include   'fortrantk.fi'
       include   'pile.fi'
       include   'msgcom.fi'
       integer   x, y,
     &           xpos,
     &           dumi
       character dum*256

       character ids*20(:,:)

       allocate(ids(58, 108), location=idsptr)

       if(idsize(n-2)) then
        dum = TkCanvasCoords('.canvas'c, ids(n-2, 1)//dn) ! x-coord from
        xpos = str2int(dum(:index(dum, '.')-1)//char(0))  ! existing pile

        do x=1, idsize(n-2)
         call TkDelete('.canvas'c, ids(n-2, x)//dn)
        end do
         else
        xpos = lxpos                            ! new pile
        lxpos = lxpos + 83                      ! 73 + 10
       end if
       call Plot(n, n-2, xpos, 360, .true., .false., .false.)

       if(idsize(n-2) .EQ. 0) then
        lxpos = lxpos - 83                      ! if pile is empty:
                                                ! make free space
        do x=7, 19 ! 6+13 - all GO piles
         if(idsize(x)) then
          dum = TkCanvasCoords('.canvas'c, ids(x, 1)//dn)  ! get x-pos of
          dumi = str2int(dum(:index(dum, '.')-1)//char(0)) ! current GO pile

          if(dumi .GT. xpos) then               ! move GO pile to left
           do y=1, idsize(x)                    ! if pile is after XPOS
            dum = ids(x, y)
            call TkTcl('.canvas move '//
     &                 dum(:ntlen(dum))//' -83 0'//dn)
           end do
          end if
         end if
        end do

         else

        ! Set new event bindings
        do x=1, idsize(n-2)
         ! Left click: get a wildcard
         call TkCanvasBind('.canvas'c, ids(n-2, x),
     &                     '<Button-1>'c, '*go_click '//int2str(n)//dn)
         ! Right click: put last card back
         call TkCanvasBind('.canvas'c, ids(n-2, x),
     &                     '<Button-3>'c, '*go_clickr '//int2str(n)//dn)
         ! Mouse over: show hint (COULD BE IMPROVED)
         call TkCanvasBind('.canvas'c, ids(n-2, x), '<Enter>'c,
     &                     '*hint '//int2str(MESSAGE_HelpMeld)//dn)
        end do
       end if
      end

* Update additional pile (meld) - redraw cards
* Opponent
      subroutine UpdateGOOp(n)
       integer   n
       include   'fortrantk.fi'
       include   'pile.fi'
       integer   x, y,
     &           xpos,
     &           dumi
       character dum*256

       character ids*20(:,:)

       allocate(ids(58, 108), location=idsptr)

       if(idsize(n-2)) then
        dum = TkCanvasCoords('.canvas'c, ids(n-2, 1)//dn) ! x-coord from
        xpos = str2int(dum(:index(dum, '.')-1)//char(0))  ! existing pile

        do x=1, idsize(n-2)
         call TkDelete('.canvas'c, ids(n-2, x)//dn)
        end do
         else
        xpos = lxposop                          ! new pile
        lxposop = lxposop + 83                  ! 73 + 10
       end if
       call Plot(n, n-2, xpos, 310, .true., .false., .true.)

       if(idsize(n-2) .EQ. 0) then
        lxposop = lxposop - 83                  ! if pile is empty:
                                                ! make free space
        do x=31, 43 ! 30+13 - all GO piles
         if(idsize(x)) then
          dum = TkCanvasCoords('.canvas'c, ids(x, 1)//dn)  ! get x-pos of
          dumi = str2int(dum(:index(dum, '.')-1)//char(0)) ! current GO pile

          if(dumi .GT. xpos) then               ! move GO pile to left
           do y=1, idsize(x)                    ! if pile is after XPOS
            dum = ids(x, y)
            call TkTcl('.canvas move '//
     &                 dum(:ntlen(dum))//' -83 0'//dn)
           end do
          end if
         end if
        end do
       end if
      end

* Update Canasta graphics - Player
* (redraw cards)
      subroutine UpdateCanasta(n)
       integer   n
       include   'fortrantk.fi'
       include   'pile.fi'
       include   'msgcom.fi'
       character dum*256

       character decks*2(:,:), ids*20(:,:)

       allocate(decks(58, 108), location=decksptr)
       allocate(ids(58, 108), location=idsptr)

       if(ntlen(ids(57, 3+n*2)) .EQ. 0) then
        call PlotTop(decks(n+19, 1), mc(n+19), size(n+19),
     &               3+n*2, 483, cypos, .true.)
        cypos = cypos + 16

        ! Bindings:
        ! left click -> Add main card from hand
        call TkCanvasBind('.canvas'c, ids(57, 4+n*2),
     &                    '<Button-1>'c, '*can_click '//int2str(n)//dn)
        ! Mouse over: Show hint
        call TkCanvasBind('.canvas'c, ids(57, 4+n*2), '<Enter>'c,
     &                    '*hint '//int2str(MESSAGE_HelpCanasta)//dn)
         else

        dum = int2str(mc(n+19))
        call TkItemConfig('.canvas'c, ids(57, 3+n*2),
     &                    '-text'c,   dum(:ntlen(dum))//'/'//
     &                                int2str(size(n+19))//dn)
       end if
      end

* Update Canasta graphics - Opponent
* (redraw cards)
      subroutine UpdateCanastaOp(n)
       integer   n
       include   'fortrantk.fi'
       include   'pile.fi'
       character dum*256

       character decks*2(:,:), ids*20(:,:)

       allocate(decks(58, 108), location=decksptr)
       allocate(ids(58, 108), location=idsptr)

       if(ntlen(ids(58, n*2)) .EQ. 0) then
        call PlotTop(decks(n+19, 1), mc(n+19), size(n+19),
     &               n*2, 483, cyposop, .false.)
        cyposop = cyposop - 16

         else

        dum = int2str(mc(n+19))
        call TkItemConfig('.canvas'c, ids(58, n*2),
     &                    '-text'c,   dum(:ntlen(dum))//'/'//
     &                                int2str(size(n+19))//dn)
       end if
      end

* Computers Turn
* Artificial "intelligence"
      subroutine EndTurn()
       include   'pile.fi'
       integer   x, y,
     &           tc,
     &           ptsw,
     &           tpos,
     &           hc, hp,
     &           dum2
       character dum*2, hcard*2
       integer   cardid,
     &           GetPoints
       logical   MoveWOT,
     &           GetTalor

       character decks*2(:,:)

       allocate(decks(58, 108), location=decksptr)

       tc = ptsw = tpos = hc =  0

       ! 1. Step: Take Talor or draw card
       ! Test whether to take it
       ! [TAKE EVERYTHING POSSIBLE YET]
       dum   = decks(2, size(2))
       dum2  = cardid(dum)
       hcard = decks(4, 1)
       do x=1, size(4)                         ! count talor-type cards,
        if(cardid(decks(4, x)) .EQ. dum2) then ! wildcard points
         tc = tc + 1
         if(tpos .EQ. 0) tpos = x
        else if(cardid(decks(4, x)) .EQ. 1) then
         ptsw = ptsw + GetPoints(decks(4, x))
        end if
                                               ! count other meldable card
        if((limit(2) .GE. 120) .AND.           ! points, too (limit >= 120)
     &     (cardid(decks(4, x)) .EQ. cardid(hcard))) then
         if(cardid(decks(4, x)) .NE. 1) hc = hc + 1
          else
         if(hc .GE. 3) ptsw = ptsw + hc*GetPoints(hcard)
         hcard = decks(4, x)
         hc = 1
        end if
       end do

       ! Take if enough talor-type cards(3), pile not frozen, point limit
       ! broken or can be broken with talor-type cards, wildcards and other
       ! meldable cards (limit >= 120)
       if((tc .GE. 2) .AND. (.NOT. blocked) .AND.
     &    (dum2 .NE. 2) .AND.
     &   ((points(2) .GE. limit(2)) .OR.
     &   ((tc+1)*GetPoints(dum)+ptsw .GE. limit(2)))) then
        do x=tpos, tpos+tc-1               ! meld all talor-type cards
         size(32+dum2) = size(32+dum2) + 1 ! anyway
         decks(32+dum2, size(32+dum2)) = decks(4, x)
        end do
        call Compress(4, tpos, tc)

        size(32+dum2) = size(32+dum2) + 1  ! meld talor top card
        decks(32+dum2, size(32+dum2)) = decks(2, size(2))
        size(2) = size(2) - 1
        mc(32+dum2) = mc(32+dum2) + tc + 1
        points(2) = points(2) + (tc+1)*GetPoints(dum)
        addpts(2) = addpts(2) + tc*GetPoints(dum)

        x  = 1
        hc = 0
        while(points(2) .LT. limit(2)) do
         ! If not enough points, fill meld with wildcards
         if(cardid(decks(4, 1)) .EQ. 1) then
          points(2) = points(2) + GetPoints(decks(4, 1))
          addpts(2) = addpts(2) + GetPoints(decks(4, 1))
          size(32+dum2) = size(32+dum2) + 1
          decks(32+dum2, size(32+dum2)) = decks(4, 1)
          call Compress(4, 1, 1)

         ! no wildcards -> meld anything possible (limit >= 120)
         else if(limit(2) .GE. 120) then
          hcard = decks(4, x)    ! [REPLACE CARDID(HCARD) ...]
          while((x .LE. size(4)) .AND.
     &          (cardid(decks(4, x)) .EQ. cardid(hcard))) do
           x = x + 1
           hc = hc + 1
          end while
          if(hc .GE. 3) then              ! >=3 cards of one type (HCARD),
           do y=x-hc, x-1                 ! -> meld these
            size(32+cardid(hcard)) = size(32+cardid(hcard)) + 1
            decks(32+cardid(hcard), size(32+cardid(hcard))) =
     &       decks(4, y)
           end do
           call Compress(4, x-hc, hc)

           mc(32+cardid(hcard)) = mc(32+cardid(hcard)) + hc
           points(2) = points(2) + hc*GetPoints(hcard)
           addpts(2) = addpts(2) + hc*GetPoints(hcard)

           call UpdateGOOp(32+cardid(hcard))     ! new GO pile -> update
           call MoveCanasta(32+cardid(hcard), 2) ! graphics
          end if
          hc = 0
         end if
        end while

        if(GetTalor(4, 6, 2))              ! take this damned talor!
     &   call UpdateOpRed()                ! and update graphics...
        call UpdateTalor()
        call UpdateGOOp(32+dum2)
        call MoveCanasta(32+dum2, 2)

         else                              ! if not, draw a card
                                           ! ...look for Red-3's...
        if(MoveWOT(pnew(6) + 1, 1, 4, 6))
     &   call UpdateOpRed()
        pnew(6) = 0
        call UpdatePile()                  ! update graphics
       end if
       call SortPile(4)
       call UpdateOpCards()

       ! 2. Step: Eventually meld more cards
       ! [MELD ANYTHING POSSIBLE YET]
       ptsw = 0
       if(points(2) .GE. limit(2)) then
        do x=1, size(4)
         dum  = decks(4, x)
         dum2 = cardid(decks(4, x))
         if((size(45+dum2) .NE. 0) .OR.
     &      (size(32+dum2) .NE. 0)) then
          if(size(45+dum2)) then
           y = 45
            else
           y = 32
          end if
          points(2) = points(2) + GetPoints(dum)
          addpts(2) = addpts(2) + GetPoints(dum)
          size(y+dum2) = size(y+dum2) + 1
          mc(y+dum2) = mc(y+dum2) + 1
          decks(y+dum2, size(y+dum2)) = dum
          call Compress(4, x, 1)
          if(y .EQ. 45) then
           call UpdateCanastaOp(dum2)
            else
           call MoveCanasta(32+dum2, 2)
           call UpdateGOOp(32+dum2)
          end if
         end if
        end do
        call UpdateOpCards()

         else

        hc = hp = tc = tpos = 0
        hcard = decks(4, 1)
        do x=1, size(4)
         if(cardid(decks(4, x)) .EQ. 1)
     &    ptsw = ptsw + GetPoints(decks(4, x))

         if(cardid(decks(4, x)) .EQ. cardid(hcard)) then
          hc = hc + 1
           else
          if((cardid(hcard) .NE. 1) .AND.
     &       (hc .GE. 3)) then
           if(limit(2) .LT. 120) then
            if(GetPoints(hcard) .GT. hp) then
             tpos = x
             tc   = hc
             hp   = hc*GetPoints(hcard)
            end if
             else
            ptsw = ptsw + GetPoints(hcard)
           end if
          end if
          hc    = 1
          hcard = decks(4, x)
         end if
        end do
        ptsw = ptsw + hp
       end if

       if(points(2)+ptsw .GE. limit(2)) then
        if(limit(2) .LT. 120) then
         do x=tpos, tpos+tc
          ! [MELD CARDS]
         end do
        end if
       end if

       drawn = .false.
      end

