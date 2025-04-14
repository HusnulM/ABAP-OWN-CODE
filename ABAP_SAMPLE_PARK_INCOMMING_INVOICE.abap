*&---------------------------------------------------------------------*
*&  Include           ZVIM002_03
*&---------------------------------------------------------------------*
class lcl_vim002_event definition final.
  public section.
    methods:
      readfilerfq,

      "LIV PO
      park_liv_po,
      delt_liv_po,
      post_liv_po,
      cncl_liv_po,

      "Invoce Claim
      park_liv_claim,
      post_liv_claim,
      delt_liv_claim,
      revs_liv_claim,

      "LIV Non PO
      park_liv_npo,
      post_liv_npo,
      delt_liv_npo,
      revs_liv_npo,

      "Down Payment
      park_dp_po,
      revs_dp_po,

      park_dp_npo,
      post_dp_npo,
      delt_dp_npo,
      revs_dp_npo,

      "Credin Note
      park_credit_note,
      post_credit_note,
      delt_credit_note,
      revs_credit_note,

      display.

    methods : run_handler for event finished of cl_gui_timer.
endclass.                    "LCL_VIM002_EVENT DEFINITION


*----------------------------------------------------------------------*
*       CLASS LCL_VIM002_EVENT IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_vim002_event implementation.
  method readfilerfq.
    clear : ls_ztvim001, src_dir.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'RFQ'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear : v_err, ls_dir.
      loop at t_dir into ls_dir.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.
        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_ekpo.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_ekpo-ebeln
                                                ls_ekpo-ebelp
                                                ls_ekpo-netpr
                                                ls_ekpo-waers
                                                ls_ekpo-aedat
                                                ls_ekpo-ltext.
                    ls_ekpo-xnetpr = ls_ekpo-netpr.
                    call function 'CONVERSION_EXIT_ALPHA_INPUT'
                      exporting
                        input  = ls_ekpo-ebelp
                      importing
                        output = ls_ekpo-ebelp.

                    if ls_ekpo is not initial.
                      clear : ls_ekpo-waers, ls_ekko.

                      select single * from ekko into ls_ekko where ebeln = ls_ekpo-ebeln.
                      select single waers from lfm1 into ls_ekpo-waers
                        where lifnr = ls_ekko-lifnr and ekorg = ls_ekko-ekorg.

                      call function 'Z_VIM_RFQ_PRICE'
                        exporting
                          rfq_number = ls_ekpo-ebeln
                          rfq_item   = ls_ekpo-ebelp
                          price      = ls_ekpo-xnetpr
                          currency   = ls_ekpo-waers
                          ptext      = ls_ekpo-ltext
                        tables
                          return     = lt_bdcmsg.

                      read table lt_bdcmsg with key msgtyp = 'E' transporting no fields.
                      if sy-subrc <> 0.
                        clear ls_ztvim002.
                        concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
                        ls_ztvim002-transtype = 'RFQ'.
                        select single lifnr from ekko into ls_ztvim002-lifnr where ebeln = ls_ekpo-ebeln.
                        ls_ztvim002-textfile  = ls_dir-name.
                        ls_ztvim002-status    = '1'.
                        ls_ztvim002-notes     = 'Price Submitted'.
                        concatenate ls_ekpo-ebeln ls_ekpo-ebelp into ls_ztvim002-refrence.
                        ls_ztvim002-erdat     = sy-datum.
                        ls_ztvim002-uname     = sy-uname.
                        modify ztvim002 from ls_ztvim002.
                        v_err = '1'.
                      else.
                        clear : ls_bdcmsg, ls_ztvim002.
                        read table lt_bdcmsg into ls_bdcmsg index 1.

*                        concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
                        select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
                        if ls_ztvim002-transid is initial.
                          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
                        endif.
                        ls_ztvim002-transtype = 'RFQ'.
                        select single lifnr from ekko into ls_ztvim002-lifnr where ebeln = ls_ekpo-ebeln.
                        ls_ztvim002-textfile  = ls_dir-name.
                        ls_ztvim002-status    = '2'.
                        ls_ztvim002-notes     = ls_bdcmsg-msgv1.
                        concatenate ls_ekpo-ebeln ls_ekpo-ebelp into ls_ztvim002-refrence.
                        ls_ztvim002-erdat     = sy-datum.
                        ls_ztvim002-uname     = sy-uname.
                        modify ztvim002 from ls_ztvim002.
                        v_err = '2'.
                      endif.
                    endif.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.
        if v_err = '1'.
          delete dataset file_source. ""delete file
        endif.

        close dataset file_source. "close file
        close dataset dst_source.  "close file
        clear ls_dir.
      endloop.

    endif.
  endmethod.                    "readfile

  method park_liv_po.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'IVPO1'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear   : ls_dir, livpo_lt_rbco.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[], livpo_returntovim-vim_iv[],
                livpo_returntovim-bkpf[], livpo_returntovim-bseg[], livpo_returntovim-rbco[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'PARK_PO' and name ns 'CLAIM'.
        refresh : lt_interface01, lt_interface01x.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_interface01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_interface01-lifnr
                                                ls_interface01-invid
                                                ls_interface01-bukrs
                                                ls_interface01-bldat
                                                ls_interface01-budat
                                                ls_interface01-bupla
                                                ls_interface01-xblnr
                                                ls_interface01-sgtxt
                                                ls_interface01-mwskz1
                                                ls_interface01-rmwwr
                                                ls_interface01-waers
                                                ls_interface01-zfbdt
                                                ls_interface01-zterm
                                                ls_interface01-zlspr
                                                ls_interface01-zuonr
                                                ls_interface01-bktxt
                                                ls_interface01-ebeln
                                                ls_interface01-ebelp
                                                ls_interface01-mblnr
                                                ls_interface01-mjahr
                                                ls_interface01-zeile
                                                ls_interface01-dmbtr
                                                ls_interface01-menge
                                                ls_interface01-meins
                                                ls_interface01-witht
                                                ls_interface01-wt_withcd
                                                ls_interface01-wt_amount
                                                ls_interface01-hkont
                                                ls_interface01-adjamount
                                                ls_interface01-adtxcode
                                                ls_interface01-adtext
                                                ls_interface01-kostl
                                                ls_interface01-shkzg
                                                .

                    ls_interface01-gsber = ls_interface01-bupla.

                    call function 'CONVERSION_EXIT_ALPHA_INPUT'
                      exporting
                        input  = ls_interface01-ebelp
                      importing
                        output = ls_interface01-ebelp.

                    call function 'CONVERSION_EXIT_ALPHA_INPUT'
                      exporting
                        input  = ls_interface01-zeile
                      importing
                        output = ls_interface01-zeile.

                    append ls_interface01 to lt_interface01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_interface01 is not initial.

        refresh : lt_interface01x, lt_interface01gl, lt_interface01wt, lt_ekbz, lt_interface01z, lt_lfbw.
        append lines of lt_interface01 to lt_interface01x.
        append lines of lt_interface01 to lt_interface01gl.
        append lines of lt_interface01 to lt_interface01wt.
        append lines of lt_interface01 to lt_interface01z.

        sort lt_interface01x  by lifnr invid.
        sort lt_interface01gl by lifnr invid hkont adjamount adtxcode kostl.
        sort lt_interface01wt by lifnr invid witht wt_withcd wt_amount.
        sort lt_interface01   by lifnr invid ebeln ebelp mblnr mjahr zeile.
        sort lt_interface01z  by lifnr invid ebeln ebelp.
        delete adjacent duplicates from lt_interface01x  comparing lifnr invid.
        delete adjacent duplicates from lt_interface01gl comparing lifnr invid hkont adjamount adtxcode kostl.
        delete adjacent duplicates from lt_interface01wt comparing lifnr invid witht wt_withcd wt_amount.
        delete adjacent duplicates from lt_interface01   comparing lifnr invid ebeln ebelp mblnr mjahr zeile.
        delete adjacent duplicates from lt_interface01z  comparing lifnr invid ebeln ebelp.


        select * from ekbz into table lt_ekbz
          for all entries in lt_interface01[]
            where ebeln = lt_interface01-ebeln and ebelp = lt_interface01-ebelp and
            belnr = lt_interface01-mblnr and gjahr = lt_interface01-mjahr and buzei = lt_interface01-zeile
            and vgabe = '1' and shkzg = 'H'.

        select * from lfbw into table lt_lfbw
          for all entries in lt_interface01[]
            where lifnr = lt_interface01-lifnr and bukrs = lt_interface01-bukrs.

        data ls_mblnr.
        clear ls_ekbz.
        loop at lt_ekbz into ls_ekbz.
          clear ls_mblnr.
          select single mblnr from mseg into ls_mblnr
            where smbln = ls_ekbz-belnr and sjahr = ls_ekbz-gjahr and smblp = ls_ekbz-buzei and bwart = '102'.
          if ls_mblnr is not initial.
            delete lt_ekbz where belnr = ls_ekbz-belnr and gjahr = ls_ekbz-gjahr and buzei = ls_ekbz-buzei.
          endif.
          clear ls_ekbz.
        endloop.

        refresh lt_ekbzx.
        clear ls_ekbz.
        loop at lt_ekbz into ls_ekbz.
          clear ls_ekbzx.
          read table lt_ekbzx into ls_ekbzx with key ebeln = ls_ekbz-ebeln ebelp = ls_ekbz-ebelp.
          if sy-subrc = 0.
            ls_ekbzx-wrbtr = ls_ekbzx-wrbtr + ls_ekbz-wrbtr.
            ls_ekbzx-dmbtr = ls_ekbzx-dmbtr + ls_ekbz-dmbtr.
            ls_ekbzx-menge = ls_ekbzx-menge + ls_ekbz-menge.

            modify lt_ekbzx from ls_ekbzx transporting dmbtr wrbtr menge where ebeln = ls_ekbz-ebeln and ebelp = ls_ekbz-ebelp.
          else.
            append ls_ekbz to lt_ekbzx.
          endif.
          clear ls_ekbz.
        endloop.

        refresh livpo_lt_taxdata.
        clear ls_interface01x.
        loop at lt_interface01x into ls_interface01x.

          clear livpo_headerdata.
          livpo_headerdata-invoice_ind    = 'X'.
          livpo_headerdata-doc_type       = 'RE'.
          livpo_headerdata-doc_date       = ls_interface01x-bldat.
          livpo_headerdata-pstng_date     = ls_interface01x-budat.
          livpo_headerdata-ref_doc_no     = ls_interface01x-xblnr.
          livpo_headerdata-comp_code      = ls_interface01x-bukrs.
          livpo_headerdata-currency       = ls_interface01x-waers.
          livpo_headerdata-bline_date     = ls_interface01x-zfbdt.
          livpo_headerdata-pmnttrms       = ls_interface01x-zterm.
          livpo_headerdata-pmnt_block     = ls_interface01x-zlspr.
          livpo_headerdata-header_txt     = ls_interface01x-bktxt.
          livpo_headerdata-alloc_nmbr     = ls_interface01x-zuonr.
          livpo_headerdata-bus_area       = ls_interface01x-gsber.
          livpo_headerdata-calc_tax_ind   = 'X'.
          livpo_headerdata-del_costs_taxc = ls_interface01x-mwskz1.
          livpo_headerdata-business_place = ls_interface01x-bupla.
          livpo_headerdata-item_text      = ls_interface01x-sgtxt.

*          data : ls_t007s type t007s.
*          clear ls_t007s.
*          select single * from t007s into ls_t007s where kalsm = 'TAXID' and mwskz = ls_interface01-mwskz1 and spras = 'EN'.

          clear ls_interface01.
          refresh : livpo_lt_itemdata, lt_cstekpo, livpo_lt_withtaxdata, livpo_lt_glaccountdata.
          data docitem type i.
          docitem = 0.

          clear : customstr, xtotalpo.
          loop at lt_interface01 into ls_interface01
            where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid.
            clear livpo_ls_itemdata.
            docitem = docitem + 1.
            livpo_ls_itemdata-invoice_doc_item = docitem.
            livpo_ls_itemdata-po_number        = ls_interface01-ebeln.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = ls_interface01-ebelp
              importing
                output = livpo_ls_itemdata-po_item.

            livpo_ls_itemdata-ref_doc          = ls_interface01-mblnr.
            livpo_ls_itemdata-ref_doc_year     = ls_interface01-mjahr.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = ls_interface01-zeile
              importing
                output = livpo_ls_itemdata-ref_doc_it.


            livpo_ls_itemdata-tax_code         = ls_interface01-mwskz1.
            livpo_ls_itemdata-quantity         = ls_interface01-menge.
            livpo_ls_itemdata-po_unit          = ls_interface01-meins.

            clear : ls_t685t, ls_ekbz.
            read table lt_ekbz into ls_ekbz
              with key ebeln = ls_interface01-ebeln ebelp = ls_interface01-ebelp
              belnr = ls_interface01-mblnr gjahr = ls_interface01-mjahr buzei = ls_interface01-zeile.

            if ls_ekbz-waers = 'IDR'.
              ls_ekbz-dmbtr = ls_ekbz-dmbtr * 100.
            endif.


            if ls_ekbz is not initial.
              ls_interface01-dmbtr      = ls_interface01-dmbtr - ls_ekbz-dmbtr.
            endif.
            condense ls_interface01-dmbtr.
            livpo_ls_itemdata-item_amount      = ls_interface01-dmbtr.

            xtotalpo = xtotalpo + ls_interface01-dmbtr.

            append livpo_ls_itemdata to livpo_lt_itemdata.

*            CONCATENATE customstr ls_interface01-ebeln into customstr.
            clear ls_ekpo.
            ls_ekpo-ebeln = ls_interface01-ebeln.
            append ls_ekpo to lt_cstekpo.
            clear ls_interface01.
          endloop.

          "Append PO Condition
          clear : ls_interface01, xtotalppkb.
          loop at lt_interface01z into ls_interface01
            where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid..
            clear : ls_t685t, ls_ekbz.
            read table lt_ekbzx into ls_ekbz
              with key ebeln = ls_interface01-ebeln ebelp = ls_interface01-ebelp.

            if ls_ekbz-waers = 'IDR'.
              ls_ekbz-dmbtr = ls_ekbz-dmbtr * 100.
            endif.

            if ls_ekbz is not initial.
              clear livpo_ls_itemdata.
              docitem = docitem + 1.
              livpo_ls_itemdata-invoice_doc_item = docitem.
              livpo_ls_itemdata-po_number        = ls_interface01-ebeln.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = ls_interface01-ebelp
                importing
                  output = livpo_ls_itemdata-po_item.

              livpo_ls_itemdata-ref_doc          = ls_interface01-mblnr.
              livpo_ls_itemdata-ref_doc_year     = ls_interface01-mjahr.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = ls_interface01-zeile
                importing
                  output = livpo_ls_itemdata-ref_doc_it.

              livpo_ls_itemdata-tax_code         = 'N0'.
              livpo_ls_itemdata-quantity         = ls_ekbz-menge.
              livpo_ls_itemdata-po_unit          = ls_interface01-meins.
              livpo_ls_itemdata-item_amount      = ls_ekbz-dmbtr.
              livpo_ls_itemdata-cond_type        = ls_ekbz-kschl.

              xtotalppkb = xtotalppkb + ls_ekbz-dmbtr.

              append livpo_ls_itemdata to livpo_lt_itemdata.
            endif.

            clear ls_interface01.
          endloop.

          "Append Adjustment Data
          clear :ls_interface01gl, docitem, xtotaladj.
          loop at lt_interface01gl into ls_interface01gl where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid.
            clear livpo_ls_glaccountdata.
            if ls_interface01gl-hkont > 0 and ls_interface01gl-kostl is not initial.
              docitem = docitem + 1.
              livpo_ls_glaccountdata-invoice_doc_item = docitem.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = ls_interface01gl-hkont
                importing
                  output = ls_interface01gl-hkont.
              if ls_interface01gl-shkzg is initial.
                ls_interface01gl-shkzg = 'S'.
              endif.
              livpo_ls_glaccountdata-gl_account       = ls_interface01gl-hkont.
              livpo_ls_glaccountdata-db_cr_ind        = ls_interface01gl-shkzg.
              livpo_ls_glaccountdata-item_amount      = ls_interface01gl-adjamount.
              livpo_ls_glaccountdata-comp_code        = ls_interface01gl-bukrs.
              livpo_ls_glaccountdata-alloc_nmbr       = ls_interface01gl-zuonr.
              livpo_ls_glaccountdata-item_text        = ls_interface01gl-adtext.
              livpo_ls_glaccountdata-tax_code         = ls_interface01gl-adtxcode.
*              call function 'CONVERSION_EXIT_ALPHA_INPUT'
*                exporting
*                  input  = ls_interface01gl-kostl
*                importing
*                  output = livpo_ls_glaccountdata-costcenter.
              clear ls_a003.
              select single * from a003 into ls_a003
                where kappl = 'TX' and kschl = 'MWVS' and aland = 'ID' and mwskz = ls_interface01gl-adtxcode.

              clear ls_konp.
              select single kbetr konwa from konp into (ls_konp-kbetr, ls_konp-konwa)
                where knumh = ls_a003-knumh.

              livpo_ls_glaccountdata-costcenter       = ls_interface01gl-kostl.
              append livpo_ls_glaccountdata to livpo_lt_glaccountdata.

              if ls_interface01gl-shkzg = 'H'.
                livpo_ls_glaccountdata-item_amount = livpo_ls_glaccountdata-item_amount * -1.
              endif.
              if ls_konp-kbetr > 0.
                ls_konp-kbetr = ( ls_konp-kbetr / 100 ) * 10.
                xtotaladj = xtotaladj + ( livpo_ls_glaccountdata-item_amount + ( livpo_ls_glaccountdata-item_amount * ( ls_konp-kbetr / 100 ) ) ).
              else.
                xtotaladj = xtotaladj + livpo_ls_glaccountdata-item_amount.
              endif.

            endif.
            clear ls_interface01gl.
          endloop.

          "Append Witholdingtax Data
          clear : ls_interface01wt, docitem.
          loop at lt_interface01wt into ls_interface01wt where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid.
            clear livpo_ls_withtaxdata.
            if ls_interface01wt-witht is not initial.
              docitem = docitem + 1.
              livpo_ls_withtaxdata-split_key   = docitem.
              livpo_ls_withtaxdata-wi_tax_type = ls_interface01wt-witht.
              livpo_ls_withtaxdata-wi_tax_code = ls_interface01wt-wt_withcd.
              livpo_ls_withtaxdata-wi_tax_base = ls_interface01wt-wt_amount.
              append livpo_ls_withtaxdata to livpo_lt_withtaxdata.
            endif.
            clear ls_interface01wt.
          endloop.

          if livpo_lt_withtaxdata is initial.
            clear ls_lfbw.
            loop at lt_lfbw into ls_lfbw where lifnr = ls_interface01x-lifnr and bukrs = ls_interface01x-bukrs.
              clear livpo_ls_withtaxdata.
              docitem = docitem + 1.
              livpo_ls_withtaxdata-split_key   = docitem.
              livpo_ls_withtaxdata-wi_tax_type = ls_lfbw-witht.
              livpo_ls_withtaxdata-wi_tax_code = ''.
              append livpo_ls_withtaxdata to livpo_lt_withtaxdata.
              clear ls_lfbw.
            endloop.
          endif.

          clear ls_interface01x.
        endloop.

*        sort lt_cstekpo by ebeln.
*        delete adjacent duplicates from lt_cstekpo comparing ebeln.
*        clear: customstr, ls_ekpo, icount, irows.
*
*        describe table lt_cstekpo lines icount.
*        loop at lt_cstekpo into ls_ekpo.
*          irows = irows + 1.
*          if irows = icount.
*            concatenate customstr ls_ekpo-ebeln into customstr.
*          else.
*            concatenate customstr ls_ekpo-ebeln ',' into customstr.
*          endif.
*          clear ls_ekpo.
*        endloop.
*        condense customstr.
*        concatenate 'Pembelian Material PO :' customstr into livpo_headerdata-item_text separated by space.

        clear ls_interface01x.
        read table lt_interface01x into ls_interface01x index 1.

        clear ls_a003.
        select single * from a003 into ls_a003
          where kappl = 'TX' and kschl = 'MWVS' and aland = 'ID' and mwskz = ls_interface01x-mwskz1.

        clear ls_konp.
        select single kbetr konwa from konp into (ls_konp-kbetr, ls_konp-konwa)
          where knumh = ls_a003-knumh.

        if ls_konp-kbetr > 0.
          ls_konp-kbetr = ( ls_konp-kbetr / 100 ) * 10.
          livpo_headerdata-gross_amount = xtotalpo + xtotaladj + ( xtotalpo * ( ls_konp-kbetr / 100 ) ).
        else.
          livpo_headerdata-gross_amount = xtotalpo + xtotaladj.
        endif.

*        if ls_interface01x-mwskz1 = 'V1' or ls_interface01x-mwskz1 = 'VA'.
*          livpo_headerdata-gross_amount = xtotalpo + xtotaladj + ( xtotalpo * ( 10 / 100 ) ).
*        elseif ls_interface01x-mwskz1 = 'V2'.
*          livpo_headerdata-gross_amount = xtotalpo + xtotaladj + ( xtotalpo * ( 1 / 100 ) ).
*        else.
*          livpo_headerdata-gross_amount = xtotalpo + xtotaladj.
*        endif.

        livpo_headerdata-gross_amount = livpo_headerdata-gross_amount + xtotalppkb.

        if ls_interface01x-waers = 'IDR'.
          livpo_headerdata-gross_amount = round( val = livpo_headerdata-gross_amount dec = 0 mode = 5 ).
        endif.

*        livpo_headerdata-bus_area    = ls_interface01x-gsber.

        if livpo_headerdata is initial.
          exit.
        endif.

        clear : livpo_invoicedocnumber, livpo_fiscalyear.
        call function 'BAPI_INCOMINGINVOICE_PARK'
          exporting
            headerdata       = livpo_headerdata
          importing
            invoicedocnumber = livpo_invoicedocnumber
            fiscalyear       = livpo_fiscalyear
          tables
            itemdata         = livpo_lt_itemdata
            accountingdata   = livpo_lt_accountingdata
            glaccountdata    = livpo_lt_glaccountdata
            taxdata          = livpo_lt_taxdata
            withtaxdata      = livpo_lt_withtaxdata
            return           = livpo_lt_return.

        if livpo_invoicedocnumber is not initial.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.


          clear xawkey.
          refresh : livpo_lt_rbkp, livpo_lt_rseg, livpo_lt_bkpf, livpo_lt_bseg, livpo_lt_rbco.

          concatenate livpo_invoicedocnumber livpo_fiscalyear into xawkey.

          clear ls_interface01x.
          read table lt_interface01x into ls_interface01x index 1.
          concatenate livpo_invoicedocnumber livpo_fiscalyear into ls_interface01x-sapdoc.
          append ls_interface01x to livpo_returntovim-vim_iv[].

*          select * from rbkp into corresponding fields of table livpo_lt_rbkp
*            where belnr = livpo_invoicedocnumber and gjahr = livpo_fiscalyear
*                  and bukrs = ls_interface01x-bukrs and lifnr = ls_interface01x-lifnr.
*
*          select * from rseg into corresponding fields of table livpo_lt_rseg
*            where belnr = livpo_invoicedocnumber and gjahr = livpo_fiscalyear
*                  and bukrs = ls_interface01x-bukrs.
*
*          select * from rbco into corresponding fields of table livpo_lt_rbco
*            where belnr = livpo_invoicedocnumber and gjahr = livpo_fiscalyear
*                  and bukrs = ls_interface01x-bukrs.
*
          select * from bkpf into corresponding fields of table livpo_lt_bkpf
            where awkey = xawkey and bukrs = ls_interface01x-bukrs.
*
*          if livpo_lt_bkpf is not initial.
*            select * from bseg into corresponding fields of table livpo_lt_bseg
*              for all entries in livpo_lt_bkpf
*                where bukrs = livpo_lt_bkpf-bukrs and belnr = livpo_lt_bkpf-belnr and gjahr = livpo_lt_bkpf-gjahr.
*          endif.
*
*          append lines of livpo_lt_rbkp to livpo_returntovim-rbkp[].
*          append lines of livpo_lt_rseg to livpo_returntovim-rseg[].
*          append lines of livpo_lt_bkpf to livpo_returntovim-bkpf[].
*          append lines of livpo_lt_bseg to livpo_returntovim-bseg[].
*          append lines of livpo_lt_rbco to livpo_returntovim-rbco[].

          refresh t_drseg.
          clear   e_rbkpv.
          call function 'MRM_INVOICE_READ'
            exporting
              i_belnr         = livpo_invoicedocnumber
              i_gjahr         = livpo_fiscalyear
            importing
              e_rbkpv         = e_rbkpv
            tables
              t_drseg         = t_drseg
            exceptions
              entry_not_found = 1
              lock_error      = 2
              others          = 3.

          loop at t_drseg assigning <fs_drseg>.
            <fs_drseg>-ok = 'X'.
          endloop.

          data :
                i_rbkpv  type  mrm_rbkpv,
                ls_xupda type  marke,
                ls_rbstat_new  type  rbstat,
                ti_drseg  type  mmcr_tdrseg.

          ls_xupda      = 'U'.
          ls_rbstat_new = 'A'.

          call function 'MRM_INVOICE_PARK'
            exporting
              i_rbkpv           = e_rbkpv
              i_xupda           = ls_xupda
              i_rbstat_new      = ls_rbstat_new
              ti_drseg          = t_drseg
            exceptions
              invalid_status    = 1
              update_impossible = 2
              user_exit         = 3
              error_message     = 99
              others            = 4.

          "Insert Log History
          clear ls_ztvim002.
          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          ls_ztvim002-transtype = 'IVPO1'.
          ls_ztvim002-lifnr     = ls_interface01x-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = 'Invoice Parked'.
          concatenate livpo_invoicedocnumber livpo_fiscalyear into ls_ztvim002-refrence.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.

          clear : ls_interface01x, livpo_ls_bkpf.
          read table lt_interface01x into ls_interface01x index 1.

          read table livpo_lt_bkpf into livpo_ls_bkpf index 1.
          concatenate livpo_invoicedocnumber livpo_fiscalyear into ls_interface01x-sapdoc.
          if ls_interface01x-sapdoc is not initial.
            submit zvim004
              with _invid  = ls_interface01x-invid
              with _sapdoc = ls_interface01x-sapdoc
              with _bukrs  = ls_interface01x-bukrs
              with _belnr  = livpo_ls_bkpf-belnr
              with _gjahr  = livpo_fiscalyear
              with _opt2   = ' '
              with _opt4   = ' '
              with _opt3   = 'X'
              with rd_ap1a = 'X' and return.
          endif.

          delete dataset file_source.
        else.
          clear livpo_ls_return.
          read table livpo_lt_return into livpo_ls_return index 1.
          "Insert Log History
          clear ls_ztvim002.
*          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'IVPO1'.
          ls_ztvim002-lifnr     = ls_interface01x-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
          ls_ztvim002-notes     = livpo_ls_return-message.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.
*          delete dataset dst_source.

        endif.

        delete dataset file_source.
        close dataset file_source. "close file
        close dataset dst_source.  "close file
        clear ls_dir.
      endloop.

      create object o_json.
      o_json->lowercase_names      = abap_true.
      o_json->include_empty_values = abap_true.
      o_json->pretty_print         = abap_true.

      concatenate api_url '/interface/livpo' into api_url.

      api_data = o_json->encode( livpo_returntovim ).
      call function 'Z_VIM_CALL_API'
        exporting
          api_data    = api_data
          api_url     = api_url
          api_method  = 'POST'
        importing
          lv_response = lv_response.
    endif.
  endmethod.                    "readfileinvpo

  method delt_liv_po.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'IVPO3'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'CNCL_PO' and name ns 'CLAIM'.
        refresh : lt_interface01, lt_interface01x.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_interface01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_interface01-invid "START OF LIV HEADER
                                                ls_interface01-lifnr
                                                ls_interface01-sapdoc
*                                                ls_interface01-bukrs
                                                ls_interface01-budat.

                    append ls_interface01 to lt_interface01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_interface01 is not initial.

        clear : livpo_invoicedocnumber, livpo_fiscalyear, ls_interface01.

        read table lt_interface01 into ls_interface01 index 1.
        livpo_invoicedocnumber = ls_interface01-sapdoc(10).
        livpo_fiscalyear       = ls_interface01-sapdoc+10(4).
        refresh livpo_lt_return.
        call function 'BAPI_INCOMINGINVOICE_DELETE'
          exporting
            invoicedocnumber = livpo_invoicedocnumber
            fiscalyear       = livpo_fiscalyear
          tables
            return           = livpo_lt_return.

        clear livpo_ls_return.
        read table livpo_lt_return into livpo_ls_return with key type = 'E'.
        if sy-subrc = 0.
          clear ls_ztvim002.
*          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'IVPO3'.
          ls_ztvim002-lifnr     = ls_interface01x-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = livpo_ls_return-message.
          concatenate livpo_invoicedocnumber livpo_fiscalyear into ls_ztvim002-refrence.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.
        else.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.

          "Insert Log History
          clear ls_ztvim002.
          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          ls_ztvim002-transtype = 'IVPO3'.
          ls_ztvim002-lifnr     = ls_interface01x-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = 'Parked Invoice Deleted'.
          concatenate livpo_invoicedocnumber livpo_fiscalyear into ls_ztvim002-refrence.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

          delete dataset file_source.
        endif.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.
    endif.
  endmethod.                    "delt_liv_po

  method cncl_liv_po.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'IVPO4'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'REVS_PO' and name ns 'CLAIM'.
        refresh : lt_interface01, lt_interface01x.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_interface01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_interface01-invid "START OF LIV HEADER
                                                ls_interface01-lifnr
                                                ls_interface01-sapdoc
                                                ls_interface01-bukrs
                                                ls_interface01-budat.

                    append ls_interface01 to lt_interface01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_interface01 is not initial.

        clear : livpo_invoicedocnumber, livpo_fiscalyear, ls_interface01.

        data reasonreversal type  bapi_incinv_fld-reason_rev.
        read table lt_interface01 into ls_interface01 index 1.
        livpo_invoicedocnumber = ls_interface01-sapdoc(10).
        livpo_fiscalyear       = ls_interface01-sapdoc+10(4).
        reasonreversal         = ls_interface01-bukrs.
        refresh livpo_lt_return.

        call function 'BAPI_INCOMINGINVOICE_CANCEL'
          exporting
            invoicedocnumber                = livpo_invoicedocnumber
            fiscalyear                      = livpo_fiscalyear
            reasonreversal                  = reasonreversal
            postingdate                     = ls_interface01-budat
*         IMPORTING
*           INVOICEDOCNUMBER_REVERSAL       =
*           FISCALYEAR_REVERSAL             =
          tables
            return                          = livpo_lt_return.

        clear livpo_ls_return.
        read table livpo_lt_return into livpo_ls_return with key type = 'E'.

        if sy-subrc = 0.
          clear ls_ztvim002.
*          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'IVPO4'.
          ls_ztvim002-lifnr     = ls_interface01x-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = livpo_ls_return-message.
          concatenate livpo_invoicedocnumber livpo_fiscalyear into ls_ztvim002-refrence.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.
        else.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.

          "Insert Log History
          clear ls_ztvim002.
          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          ls_ztvim002-transtype = 'IVPO4'.
          ls_ztvim002-lifnr     = ls_interface01x-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = 'Posted document reversed'.
          concatenate livpo_invoicedocnumber livpo_fiscalyear into ls_ztvim002-refrence.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

*          data xawkey type awkey.
*          clear xawkey.
*          refresh : livpo_lt_rbkp, livpo_lt_rseg, livpo_lt_bkpf, livpo_lt_bseg, livpo_lt_rbco.
*          xawkey = ls_interface01x-sapdoc.
*
*          append ls_interface01x to livpo_returntovim-vim_iv[].
*
*          select * from rbkp into corresponding fields of table livpo_lt_rbkp
*            where belnr = livpo_invoicedocnumber and gjahr = livpo_fiscalyear
*                  and lifnr = ls_interface01x-lifnr.
*
*          select * from rseg into corresponding fields of table livpo_lt_rseg
*            where belnr = livpo_invoicedocnumber and gjahr = livpo_fiscalyear.
*
*          select * from rbco into corresponding fields of table livpo_lt_rbco
*            where belnr = livpo_invoicedocnumber and gjahr = livpo_fiscalyear.
*
*          select * from bkpf into corresponding fields of table livpo_lt_bkpf
*            where awkey = xawkey.
*
*          if livpo_lt_bkpf is not initial.
*            select * from bseg into corresponding fields of table livpo_lt_bseg
*              for all entries in livpo_lt_bkpf
*                where bukrs = livpo_lt_bkpf-bukrs and belnr = livpo_lt_bkpf-belnr and gjahr = livpo_lt_bkpf-gjahr.
*          endif.
*
*          append lines of livpo_lt_rbkp to livpo_returntovim-rbkp[].
*          append lines of livpo_lt_rseg to livpo_returntovim-rseg[].
*          append lines of livpo_lt_bkpf to livpo_returntovim-bkpf[].
*          append lines of livpo_lt_bseg to livpo_returntovim-bseg[].
*          append lines of livpo_lt_rbco to livpo_returntovim-rbco[].

          delete dataset file_source.
        endif.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.

*      if livpo_returntovim is not initial.
*        create object o_json.
*        o_json->lowercase_names      = abap_true.
*        o_json->include_empty_values = abap_true.
*        o_json->pretty_print         = abap_true.
*
*        concatenate api_url '/interface/livpo' into api_url.
*
*        api_data = o_json->encode( livpo_returntovim ).
*        call function 'Z_VIM_CALL_API'
*          exporting
*            api_data    = api_data
*            api_url     = api_url
*            api_method  = 'POST'
*          importing
*            lv_response = lv_response.
*      endif.
    endif.
  endmethod.                    "cncl_liv_po

  method post_liv_po.
*    data xawkey type awkey.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'IVPO2'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear : ls_dir, livpo_returntovim.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[], livpo_returntovim-vim_iv[],
                livpo_returntovim-bkpf[], livpo_returntovim-bseg[], livpo_returntovim-rbco[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'POST_PO' and name ns 'CLAIM'.
        refresh : lt_interface01, lt_interface01x.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_interface01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_interface01-lifnr
                                                ls_interface01-invid
                                                ls_interface01-bukrs
                                                ls_interface01-bldat
                                                ls_interface01-budat
                                                ls_interface01-bupla
                                                ls_interface01-xblnr
                                                ls_interface01-sgtxt
                                                ls_interface01-mwskz1
                                                ls_interface01-rmwwr
                                                ls_interface01-waers
                                                ls_interface01-zfbdt
                                                ls_interface01-zterm
                                                ls_interface01-zlspr
                                                ls_interface01-zuonr
                                                ls_interface01-bktxt
                                                ls_interface01-sapdoc

                                                ls_interface01-ebeln
                                                ls_interface01-ebelp
                                                ls_interface01-mblnr
                                                ls_interface01-mjahr
                                                ls_interface01-zeile
                                                ls_interface01-dmbtr
                                                ls_interface01-menge
                                                ls_interface01-meins

                                                ls_interface01-witht
                                                ls_interface01-wt_withcd
                                                ls_interface01-wt_amount
                                                ls_interface01-hkont
                                                ls_interface01-adjamount
                                                ls_interface01-adtxcode
                                                ls_interface01-adtext
                                                ls_interface01-kostl
                                                ls_interface01-shkzg
                                                .

                    ls_interface01-gsber = ls_interface01-bupla.
                    call function 'CONVERSION_EXIT_ALPHA_INPUT'
                      exporting
                        input  = ls_interface01-ebelp
                      importing
                        output = ls_interface01-ebelp.

                    call function 'CONVERSION_EXIT_ALPHA_INPUT'
                      exporting
                        input  = ls_interface01-zeile
                      importing
                        output = ls_interface01-zeile.
                    append ls_interface01 to lt_interface01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_interface01 is not initial.

        refresh : lt_interface01x, lt_interface01gl, lt_interface01wt, lt_ekbz, lt_interface01z.
        append lines of lt_interface01 to lt_interface01x.
        append lines of lt_interface01 to lt_interface01gl.
        append lines of lt_interface01 to lt_interface01wt.
        append lines of lt_interface01 to lt_interface01z.

        sort lt_interface01x  by lifnr invid.
        sort lt_interface01gl by lifnr invid hkont adjamount adtxcode kostl.
        sort lt_interface01wt by lifnr invid witht wt_withcd wt_amount.
        sort lt_interface01   by lifnr invid ebeln ebelp mblnr mjahr zeile.
        sort lt_interface01z  by lifnr invid ebeln ebelp.
        delete adjacent duplicates from lt_interface01x  comparing lifnr invid.
        delete adjacent duplicates from lt_interface01gl comparing lifnr invid hkont adjamount adtxcode kostl.
        delete adjacent duplicates from lt_interface01wt comparing lifnr invid witht wt_withcd wt_amount.
        delete adjacent duplicates from lt_interface01   comparing lifnr invid ebeln ebelp mblnr mjahr zeile.
        delete adjacent duplicates from lt_interface01z  comparing lifnr invid ebeln ebelp.


        select * from ekbz into table lt_ekbz
          for all entries in lt_interface01[]
            where ebeln = lt_interface01-ebeln and ebelp = lt_interface01-ebelp and
            belnr = lt_interface01-mblnr and gjahr = lt_interface01-mjahr and buzei = lt_interface01-zeile
            and vgabe = '1' and shkzg = 'H'.

        refresh lt_lfbw.
        select * from lfbw into table lt_lfbw
          for all entries in lt_interface01[]
            where lifnr = lt_interface01-lifnr and bukrs = lt_interface01-bukrs.

        data ls_mblnr.
        clear ls_ekbz.
        loop at lt_ekbz into ls_ekbz.
          clear ls_mblnr.
          select single mblnr from mseg into ls_mblnr
            where smbln = ls_ekbz-belnr and sjahr = ls_ekbz-gjahr and smblp = ls_ekbz-buzei and bwart = '102'.
          if ls_mblnr is not initial.
            delete lt_ekbz where belnr = ls_ekbz-belnr and gjahr = ls_ekbz-gjahr and buzei = ls_ekbz-buzei.
          endif.
          clear ls_ekbz.
        endloop.

        refresh lt_ekbzx.
        clear ls_ekbz.
        loop at lt_ekbz into ls_ekbz.
          clear ls_ekbzx.
          read table lt_ekbzx into ls_ekbzx with key ebeln = ls_ekbz-ebeln ebelp = ls_ekbz-ebelp.
          if sy-subrc = 0.
            ls_ekbzx-wrbtr = ls_ekbzx-wrbtr + ls_ekbz-wrbtr.
            ls_ekbzx-dmbtr = ls_ekbzx-dmbtr + ls_ekbz-dmbtr.
            ls_ekbzx-menge = ls_ekbzx-menge + ls_ekbz-menge.

            modify lt_ekbzx from ls_ekbzx transporting dmbtr wrbtr menge where ebeln = ls_ekbz-ebeln and ebelp = ls_ekbz-ebelp.
          else.
            append ls_ekbz to lt_ekbzx.
          endif.
          clear ls_ekbz.
        endloop.

        refresh livpo_lt_taxdata.

        clear ls_interface01x.
        loop at lt_interface01x into ls_interface01x.
          data :
              i_belnr  type rbkp_v-belnr,
              i_gjahr  type rbkp_v-gjahr,
              i_belnr2  type bkpf-belnr,
              i_gjahr2  type bkpf-gjahr,
              e_output type string,
              e_msgtyp type string.

          clear : i_belnr, i_gjahr, e_output.
          i_belnr = ls_interface01x-sapdoc(10).
          i_gjahr = ls_interface01x-sapdoc+10(4).

          refresh livpo_lt_return.
          call function 'BAPI_INCOMINGINVOICE_GETDETAIL'
            exporting
              invoicedocnumber = i_belnr
              fiscalyear       = i_gjahr
            importing
              headerdata       = headerdata_dtl
            tables
              itemdata         = lt_itemdata_dtl
              accountingdata   = lt_accountingdata_dtl
              glaccountdata    = lt_glaccountdata_dtl
              taxdata          = lt_taxdata_dtl
              withtaxdata      = lt_withtaxdata_dtl
              return           = livpo_lt_return.


          refresh livpo_lt_glaccountdata.
          if sy-subrc = 0.

            clear ls_interface01.
            refresh : livpo_lt_itemdata, lt_cstekpo, livpo_lt_withtaxdata, livpo_lt_glaccountdata.
            data docitem type i.
            docitem = 0.

            clear : customstr, xtotalpo.
            loop at lt_interface01 into ls_interface01
              where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid.
              clear livpo_ls_itemdata.
              docitem = docitem + 1.
              livpo_ls_itemdata-invoice_doc_item = docitem.
              livpo_ls_itemdata-po_number        = ls_interface01-ebeln.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = ls_interface01-ebelp
                importing
                  output = livpo_ls_itemdata-po_item.

              livpo_ls_itemdata-ref_doc          = ls_interface01-mblnr.
              livpo_ls_itemdata-ref_doc_year     = ls_interface01-mjahr.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = ls_interface01-zeile
                importing
                  output = livpo_ls_itemdata-ref_doc_it.

              livpo_ls_itemdata-tax_code         = ls_interface01-mwskz1.
              livpo_ls_itemdata-quantity         = ls_interface01-menge.
              livpo_ls_itemdata-po_unit          = ls_interface01-meins.

              clear : ls_t685t, ls_ekbz.
              read table lt_ekbz into ls_ekbz
                with key ebeln = ls_interface01-ebeln ebelp = ls_interface01-ebelp
                belnr = ls_interface01-mblnr gjahr = ls_interface01-mjahr buzei = ls_interface01-zeile.

              if ls_ekbz-waers = 'IDR'.
                ls_ekbz-dmbtr = ls_ekbz-dmbtr * 100.
              endif.


              if ls_ekbz is not initial.
                ls_interface01-dmbtr      = ls_interface01-dmbtr - ls_ekbz-dmbtr.
              endif.
              xtotalpo = xtotalpo + ls_interface01-dmbtr.
              condense ls_interface01-dmbtr.
              livpo_ls_itemdata-item_amount      = ls_interface01-dmbtr.
              append livpo_ls_itemdata to livpo_lt_itemdata.

              clear ls_ekpo.
              ls_ekpo-ebeln = ls_interface01-ebeln.
              append ls_ekpo to lt_cstekpo.
              clear ls_interface01.
            endloop.

            "Append PO Condition
            clear : ls_interface01, xtotalppkb.
            loop at lt_interface01z into ls_interface01
              where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid..
              clear : ls_t685t, ls_ekbz.
              read table lt_ekbzx into ls_ekbz
                with key ebeln = ls_interface01-ebeln ebelp = ls_interface01-ebelp.

              if ls_ekbz-waers = 'IDR'.
                ls_ekbz-dmbtr = ls_ekbz-dmbtr * 100.
              endif.

              if ls_ekbz is not initial.
                clear livpo_ls_itemdata.
                docitem = docitem + 1.
                livpo_ls_itemdata-invoice_doc_item = docitem.
                livpo_ls_itemdata-po_number        = ls_interface01-ebeln.
                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = ls_interface01-ebelp
                  importing
                    output = livpo_ls_itemdata-po_item.

                livpo_ls_itemdata-ref_doc          = ls_interface01-mblnr.
                livpo_ls_itemdata-ref_doc_year     = ls_interface01-mjahr.
                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = ls_interface01-zeile
                  importing
                    output = livpo_ls_itemdata-ref_doc_it.

                livpo_ls_itemdata-tax_code         = 'N0'.
                livpo_ls_itemdata-quantity         = ls_ekbz-menge.
                livpo_ls_itemdata-po_unit          = ls_interface01-meins.
                livpo_ls_itemdata-item_amount      = ls_ekbz-dmbtr.
                livpo_ls_itemdata-cond_type        = ls_ekbz-kschl.
                xtotalppkb = xtotalppkb + ls_ekbz-dmbtr.

                append livpo_ls_itemdata to livpo_lt_itemdata.
              endif.

              clear ls_interface01.
            endloop.

            "Append Adjustment Data
            refresh livpo_lt_glaccountdata.
            clear :ls_interface01gl, docitem, xtotaladj.
            loop at lt_interface01gl into ls_interface01gl where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid.
              clear livpo_ls_glaccountdata.
              if ls_interface01gl-hkont > 0 and ls_interface01gl-kostl is not initial.
                docitem = docitem + 1.
                livpo_ls_glaccountdata-invoice_doc_item = docitem.
                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = ls_interface01gl-hkont
                  importing
                    output = ls_interface01gl-hkont.

                if ls_interface01gl-shkzg is initial.
                  ls_interface01gl-shkzg = 'S'.
                endif.
                livpo_ls_glaccountdata-gl_account       = ls_interface01gl-hkont.
                livpo_ls_glaccountdata-db_cr_ind        = ls_interface01gl-shkzg.
                livpo_ls_glaccountdata-item_amount      = ls_interface01gl-adjamount.
                livpo_ls_glaccountdata-comp_code        = ls_interface01gl-bukrs.
                livpo_ls_glaccountdata-alloc_nmbr       = ls_interface01gl-zuonr.
                livpo_ls_glaccountdata-item_text        = ls_interface01gl-adtext.
                livpo_ls_glaccountdata-tax_code         = ls_interface01gl-adtxcode.

                clear ls_a003.
                select single * from a003 into ls_a003
                  where kappl = 'TX' and kschl = 'MWVS' and aland = 'ID' and mwskz = ls_interface01gl-adtxcode.

                clear ls_konp.
                select single kbetr konwa from konp into (ls_konp-kbetr, ls_konp-konwa)
                  where knumh = ls_a003-knumh.

                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = ls_interface01gl-kostl
                  importing
                    output = livpo_ls_glaccountdata-costcenter.

                append livpo_ls_glaccountdata to livpo_lt_glaccountdata.

                if ls_interface01gl-shkzg = 'H'.
                  livpo_ls_glaccountdata-item_amount = livpo_ls_glaccountdata-item_amount * -1.
                endif.
                if ls_konp-kbetr > 0.
                  ls_konp-kbetr = ( ls_konp-kbetr / 100 ) * 10.
                  xtotaladj = xtotaladj + ( livpo_ls_glaccountdata-item_amount + ( livpo_ls_glaccountdata-item_amount * ( ls_konp-kbetr / 100 ) ) ).
                else.
                  xtotaladj = xtotaladj + livpo_ls_glaccountdata-item_amount.
                endif.
              endif.
              clear ls_interface01gl.
            endloop.

            "Append Witholdingtax Data
            refresh livpo_lt_withtaxdata.
            clear : ls_interface01wt, docitem.
            loop at lt_interface01wt into ls_interface01wt where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid.
              clear livpo_ls_withtaxdata.
              if ls_interface01wt-witht is not initial.
                docitem = docitem + 1.
                livpo_ls_withtaxdata-split_key   = docitem.
                livpo_ls_withtaxdata-wi_tax_type = ls_interface01wt-witht.
                livpo_ls_withtaxdata-wi_tax_code = ls_interface01wt-wt_withcd.
                livpo_ls_withtaxdata-wi_tax_base = ls_interface01wt-wt_amount.
                append livpo_ls_withtaxdata to livpo_lt_withtaxdata.
              endif.
              clear ls_interface01wt.
            endloop.

            if livpo_lt_withtaxdata is initial.
              clear ls_lfbw.
              loop at lt_lfbw into ls_lfbw where lifnr = ls_interface01x-lifnr and bukrs = ls_interface01x-bukrs.
                clear livpo_ls_withtaxdata.
                docitem = docitem + 1.
                livpo_ls_withtaxdata-split_key   = docitem.
                livpo_ls_withtaxdata-wi_tax_type = ls_lfbw-witht.
                livpo_ls_withtaxdata-wi_tax_code = ''.
                append livpo_ls_withtaxdata to livpo_lt_withtaxdata.
                clear ls_lfbw.
              endloop.
            endif.

            sort lt_cstekpo by ebeln.
            delete adjacent duplicates from lt_cstekpo comparing ebeln.
            clear: customstr, ls_ekpo, icount, irows.

*            describe table lt_cstekpo lines icount.
*            loop at lt_cstekpo into ls_ekpo.
*              irows = irows + 1.
*              if irows = icount.
*                concatenate customstr ls_ekpo-ebeln into customstr.
*              else.
*                concatenate customstr ls_ekpo-ebeln ',' into customstr.
*              endif.
*              clear ls_ekpo.
*            endloop.
*            condense customstr.
*            concatenate 'Pembelian Material PO :' customstr into livpo_headerdata-item_text separated by space.

            clear ls_interface01x.
            read table lt_interface01x into ls_interface01x index 1.

            clear ls_a003.
            select single * from a003 into ls_a003
              where kappl = 'TX' and kschl = 'MWVS' and aland = 'ID' and mwskz = ls_interface01x-mwskz1.

            clear ls_konp.
            select single kbetr konwa from konp into (ls_konp-kbetr, ls_konp-konwa)
              where knumh = ls_a003-knumh.

            if ls_konp-kbetr > 0.
              ls_konp-kbetr = ( ls_konp-kbetr / 100 ) * 10.
              livpo_headerdata-gross_amount = xtotalpo + xtotaladj + ( xtotalpo * ( ls_konp-kbetr / 100 ) ).
            else.
              livpo_headerdata-gross_amount = xtotalpo + xtotaladj.
            endif.

            livpo_headerdata-gross_amount = livpo_headerdata-gross_amount + xtotalppkb.

            if ls_interface01x-waers = 'IDR'.
              livpo_headerdata-gross_amount = round( val = livpo_headerdata-gross_amount dec = 0 mode = 5 ).
            endif.

            refresh livpo_lt_return.
            clear : headerdata_change, headerdata_changex, table_change.
            move-corresponding headerdata_dtl to headerdata_change.

            headerdata_change-pstng_date     = ls_interface01x-budat.
            headerdata_change-bline_date     = ls_interface01x-zfbdt.
            headerdata_change-doc_date       = ls_interface01x-bldat.
            headerdata_change-ref_doc_no     = ls_interface01x-xblnr.
            headerdata_change-currency       = ls_interface01x-waers.
            headerdata_change-gross_amount   = livpo_headerdata-gross_amount.
            headerdata_change-bline_date     = ls_interface01x-zfbdt.
            headerdata_change-pmnttrms       = ls_interface01x-zterm.
            headerdata_change-pmnt_block     = ls_interface01x-zlspr.
            headerdata_change-header_txt     = ls_interface01x-bktxt.
            headerdata_change-alloc_nmbr     = ls_interface01x-zuonr.
            headerdata_change-bus_area       = ls_interface01x-gsber.
            headerdata_change-calc_tax_ind   = 'X'.
            headerdata_change-del_costs_taxc = ls_interface01x-mwskz1.
            headerdata_change-business_place = ls_interface01x-bupla.

            headerdata_changex-pstng_date = 'X'.
            headerdata_changex-bline_date = 'X'.
            headerdata_changex-doc_date   = 'X'.

            headerdata_changex-ref_doc_no   = 'X'.
            headerdata_changex-currency     = 'X'.
            headerdata_changex-gross_amount = 'X'.

            headerdata_changex-bline_date  = 'X'.
            headerdata_changex-pmnttrms    = 'X'.
            headerdata_changex-pmnt_block  = 'X'.
            headerdata_changex-header_txt  = 'X'.
            headerdata_changex-alloc_nmbr  = 'X'.
            headerdata_changex-bus_area    = 'X'.
            headerdata_changex-calc_tax_ind   = 'X'.
            headerdata_changex-del_costs_taxc = 'X'.
            headerdata_changex-business_place = 'X'.

            if livpo_lt_glaccountdata is not initial.
              table_change-glaccountdata    = 'X'.
              table_change-withtaxdata      = 'X'.
            endif.

            call function 'BAPI_INCOMINGINVOICE_CHANGE'
              exporting
                invoicedocnumber   = i_belnr
                fiscalyear         = i_gjahr
                table_change       = table_change
                headerdata_change  = headerdata_change
                headerdata_changex = headerdata_changex
              tables
*               itemdata           = livpo_lt_itemdata
*               accountingdata     = livpo_lt_accountingdata
                glaccountdata      = livpo_lt_glaccountdata
*               taxdata            = livpo_lt_taxdata
                withtaxdata        = livpo_lt_withtaxdata
                return             = livpo_lt_return.

            clear livpo_ls_return.
            read table livpo_lt_return into livpo_ls_return with key type = 'E'.

            if sy-subrc = 0.
              clear : ls_ztvim002, livpo_ls_return.
              read table livpo_lt_return into livpo_ls_return index 1.
*              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
              select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
              if ls_ztvim002-transid is initial.
                concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
              endif.
              ls_ztvim002-transtype = 'IVPO2'.
              ls_ztvim002-lifnr     = ls_interface01x-lifnr.
              ls_ztvim002-textfile  = ls_dir-name.
              ls_ztvim002-status    = '2'.
              ls_ztvim002-notes     = livpo_ls_return-message.
              ls_ztvim002-refrence  = ls_interface01x-sapdoc.
              ls_ztvim002-notes     = livpo_ls_return-message.
              ls_ztvim002-erdat     = sy-datum.
              ls_ztvim002-itime     = sy-uzeit.
              ls_ztvim002-uname     = sy-uname.
              modify ztvim002 from ls_ztvim002.
            else.
              call function 'BAPI_TRANSACTION_COMMIT'
                exporting
                  wait = 'X'.

              refresh livpo_lt_return.
              call function 'BAPI_INCOMINGINVOICE_POST'
                exporting
                  invoicedocnumber = i_belnr
                  fiscalyear       = i_gjahr
                tables
                  return           = livpo_lt_return.

              if livpo_lt_return is initial.
                call function 'BAPI_TRANSACTION_COMMIT'
                  exporting
                    wait = 'X'.

                "Insert Log History
                clear ls_ztvim002.
                concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
                ls_ztvim002-transtype = 'IVPO2'.
                ls_ztvim002-lifnr     = ls_interface01x-lifnr.
                ls_ztvim002-textfile  = ls_dir-name.
                ls_ztvim002-status    = '1'.
                ls_ztvim002-notes     = 'Invoice Posted'.
                ls_ztvim002-refrence  = ls_interface01x-sapdoc.
                ls_ztvim002-erdat     = sy-datum.
                ls_ztvim002-itime     = sy-uzeit.
                ls_ztvim002-uname     = sy-uname.
                modify ztvim002 from ls_ztvim002.

                clear xawkey.
                refresh : livpo_lt_rbkp, livpo_lt_rseg, livpo_lt_bkpf, livpo_lt_bseg, livpo_lt_rbco.
                xawkey = ls_interface01x-sapdoc.

                append ls_interface01x to livpo_returntovim-vim_iv[].

                clear ls_interface01x.

                delete dataset file_source.
              else.
                call function 'BAPI_TRANSACTION_ROLLBACK'.
                clear livpo_ls_return.
                read table livpo_lt_return into livpo_ls_return with key type = 'E'.
                clear : ls_ztvim002, livpo_ls_return.
                read table livpo_lt_return into livpo_ls_return index 1.
                concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
                ls_ztvim002-transtype = 'IVPO2'.
                ls_ztvim002-lifnr     = ls_interface01x-lifnr.
                ls_ztvim002-textfile  = ls_dir-name.
                ls_ztvim002-status    = '2'.
                ls_ztvim002-notes     = livpo_ls_return-message.
                ls_ztvim002-refrence  = ls_interface01x-sapdoc.
                ls_ztvim002-notes     = livpo_ls_return-message.
                ls_ztvim002-erdat     = sy-datum.
                ls_ztvim002-itime     = sy-uzeit.
                ls_ztvim002-uname     = sy-uname.
                modify ztvim002 from ls_ztvim002.

*                delete dataset dst_source.
              endif.
            endif.
          endif.
        endloop.

        delete dataset file_source.
        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.

      if livpo_returntovim is not initial.
        create object o_json.
        o_json->lowercase_names      = abap_true.
        o_json->include_empty_values = abap_true.
        o_json->pretty_print         = abap_true.

        concatenate api_url '/interface/livpo' into api_url.

        api_data = o_json->encode( livpo_returntovim ).
        call function 'Z_VIM_CALL_API'
          exporting
            api_data    = api_data
            api_url     = api_url
            api_method  = 'POST'
          importing
            lv_response = lv_response.
      endif.

    endif.
  endmethod.                    "post_liv_po

  method park_liv_claim.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'CLM1'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'PARK_PO_CLAIM'.
        refresh : lt_liv_npo1, lt_liv_npo2.
        clear: file_source.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.
        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_liv_npo1.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_liv_npo1-invid
                                                ls_liv_npo1-lifnr
                                                ls_liv_npo1-bukrs
                                                ls_liv_npo1-bldat
                                                ls_liv_npo1-budat
                                                ls_liv_npo1-bupla
                                                ls_liv_npo1-xblnr
                                                ls_liv_npo1-sgtxt
                                                ls_liv_npo1-mwskz1
                                                ls_liv_npo1-rmwwr
                                                ls_liv_npo1-waers
                                                ls_liv_npo1-zfbdt
                                                ls_liv_npo1-zterm
                                                ls_liv_npo1-zlspr
                                                ls_liv_npo1-zuonr
                                                ls_liv_npo1-bktxt
                                                ls_liv_npo1-witht
                                                ls_liv_npo1-wt_withcd
                                                ls_liv_npo1-wt_amount
                                                ls_liv_npo1-hkont
                                                ls_liv_npo1-adjamount
                                                ls_liv_npo1-adtxcode
                                                ls_liv_npo1-adtext
                                                ls_liv_npo1-kostl
                                                ls_liv_npo1-sapdoc.

                    append ls_liv_npo1 to lt_liv_npo1.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_liv_npo1 is not initial.

        data :
          lt_glaccount type table of zsvim_livnpo_gl,
          ls_glaccount type zsvim_livnpo_gl,
          lt_wihtax    type table of zsvim_livnpo_wht,
          ls_wihtax    type zsvim_livnpo_wht,
          lt_return    type table of bdcmsgcoll,
          ls_return    type bdcmsgcoll.

        refresh : lt_glaccount, lt_wihtax, lt_liv_npo2, lt_liv_npo3.

        append lines of lt_liv_npo1 to lt_liv_npo2.
        append lines of lt_liv_npo1 to lt_liv_npo3.

        sort lt_liv_npo1 by invid lifnr bukrs.
        sort lt_liv_npo2 by invid lifnr bukrs hkont adjamount adtxcode adtext bupla kostl.
        sort lt_liv_npo3 by invid lifnr bukrs witht.

        delete adjacent duplicates from lt_liv_npo1 comparing invid lifnr bukrs.
        delete adjacent duplicates from lt_liv_npo3 comparing invid lifnr bukrs witht.
        delete adjacent duplicates from lt_liv_npo2 comparing invid lifnr bukrs hkont adjamount adtxcode adtext bupla kostl.

        refresh lt_lfbw.
        select * from lfbw into table lt_lfbw
          for all entries in lt_liv_npo1[]
            where lifnr = lt_liv_npo1-lifnr and bukrs = lt_liv_npo1-bukrs.

        clear ls_liv_npo1.
        loop at lt_liv_npo1 into ls_liv_npo1.

          "Append GL Data
          clear : ls_liv_npo2, xtotaladj.
          loop at lt_liv_npo2 into ls_liv_npo2 where invid = ls_liv_npo1-invid and lifnr = ls_liv_npo1-lifnr.
            clear ls_glaccount.
            move-corresponding ls_liv_npo2 to ls_glaccount.
            ls_glaccount-gsber = ls_liv_npo1-bupla.
            append ls_glaccount to lt_glaccount.
            xtotaladj = xtotaladj + ls_glaccount-adjamount.
            clear ls_liv_npo2.
          endloop.

          "Witholding Tax data
          clear ls_liv_npo3.
          loop at lt_liv_npo3 into ls_liv_npo3 where invid = ls_liv_npo1-invid and lifnr = ls_liv_npo1-lifnr.
            clear ls_wihtax.
            move-corresponding ls_liv_npo3 to ls_wihtax.
            if ls_wihtax-witht is not initial.
              append ls_wihtax to lt_wihtax.
            endif.
            clear ls_liv_npo3.
          endloop.

          if lt_wihtax is initial.
            clear ls_lfbw.
            loop at lt_lfbw into ls_lfbw where lifnr = ls_liv_npo1-lifnr and bukrs = ls_liv_npo1-bukrs.
              clear ls_wihtax.
              ls_wihtax-witht     = ls_lfbw-witht.
              ls_wihtax-wt_withcd = ' '.
              append ls_wihtax to lt_wihtax.
              clear ls_lfbw.
            endloop.
          endif.

          clear ls_liv_npo1.
        endloop.

        clear ls_liv_npo1.
        read table lt_liv_npo1 into ls_liv_npo1 index 1.

        check ls_liv_npo1 is not initial.
        clear ls_liv_npo1-rmwwr.
        if ls_liv_npo1-waers = 'IDR'.
          ls_liv_npo1-rmwwr = round( val = xtotaladj dec = 0 mode = 5 ).
        else.
          ls_liv_npo1-rmwwr = xtotaladj.
        endif.
        condense ls_liv_npo1-rmwwr.

*        clear ls_liv_npo1-rmwwr.
*        ls_liv_npo1-rmwwr = round( val = xtotaladj dec = 0 mode = 5 ).
*        condense ls_liv_npo1-rmwwr.
        data v_kunnr type kna1-kunnr.
        clear v_kunnr.
        select single kunnr from lfa1 into v_kunnr where lifnr = ls_liv_npo1-lifnr.
        clear ls_liv_npo1-lifnr.
        ls_liv_npo1-lifnr = v_kunnr.
        clear v_kunnr.
        select single kunnr from knb1 into v_kunnr where kunnr = ls_liv_npo1-lifnr and bukrs = ls_liv_npo1-bukrs.

        if v_kunnr is not initial.
          clear ls_liv_npo1-lifnr.
          ls_liv_npo1-lifnr = v_kunnr.
          refresh lt_return.
          call function 'Z_VIM_PARK_FV70'
            exporting
              i_header     = ls_liv_npo1
            tables
              lt_glaccount = lt_glaccount
              lt_wihtax    = lt_wihtax
              lt_return    = lt_return.

          wait up to 1 seconds.

          clear ls_return.
          read table lt_return into ls_return with key msgtyp = 'E'.
          if sy-subrc <> 0.
            call function 'BAPI_TRANSACTION_COMMIT'
              exporting
                wait = 'X'.

            delete lt_return where msgid <> 'FP'.
            delete lt_return where msgv1 = ''.

            clear ls_return.
            read table lt_return into ls_return with key msgid = 'FP'.
            if ls_return-msgv1 is not initial.
              clear xawkey.
              select single awkey from bkpf into xawkey
                where bukrs = ls_liv_npo1-bukrs and belnr = ls_return-msgv1 and gjahr = ls_liv_npo1-budat(4).

              "Insert Log History
              clear ls_ztvim002.
              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
              ls_ztvim002-transtype = 'CLM1'.
              ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
              ls_ztvim002-textfile  = ls_dir-name.
              ls_ztvim002-status    = '1'.
              ls_ztvim002-notes     = 'Invoice Claim Parked'.
              ls_ztvim002-refrence  = xawkey.
              ls_ztvim002-erdat     = sy-datum.
              ls_ztvim002-itime     = sy-uzeit.
              ls_ztvim002-uname     = sy-uname.
              modify ztvim002 from ls_ztvim002.

              clear ls_interface01x.
              ls_interface01x-invid  = ls_liv_npo1-invid.
              ls_interface01x-sapdoc = xawkey.
              append ls_interface01x to livpo_returntovim-vim_iv[].

              submit zvim004
                with _invid  = ls_liv_npo1-invid
                with _sapdoc = xawkey
                with _bukrs  = ls_liv_npo1-bukrs
                with _belnr  = ls_return-msgv1
                with _gjahr  = ls_liv_npo1-budat(4)
                with _opt2   = 'X'
                with _opt4   = ' '
                with _opt3   = ' '
                with rd_ap1a = ' ' and return.

              delete dataset file_source.
            endif.
          else.
            "BAPI rollback
*            call function 'BAPI_TRANSACTION_ROLLBACK'.
            "Insert Log History
            data :
                msgno type symsgno.
            clear msgno.
            msgno = ls_return-msgnr.

            clear ls_ztvim002.
            call function 'FORMAT_MESSAGE'
              exporting
                id        = ls_return-msgid
                lang      = '-D'
                no        = msgno
                v1        = ls_return-msgv1
                v2        = ls_return-msgv2
                v3        = ls_return-msgv3
                v4        = ls_return-msgv4
              importing
                msg       = ls_ztvim002-notes
              exceptions
                not_found = 1
                others    = 2.

*            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
            if ls_ztvim002-transid is initial.
              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            endif.
            ls_ztvim002-transtype = 'CLM1'.
            ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '2'.
*          ls_ztvim002-notes     = 'Invoice Clam Parked'.
*          ls_ztvim002-refrence  = xawkey.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.

            delete dataset dst_source.
          endif.
        else.
          clear ls_ztvim002.
          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          ls_ztvim002-transtype = 'CLM1'.
          ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
          ls_ztvim002-notes     = 'Please maintain customer code for vendor'.
*            ls_ztvim002-refrence  = xawkey.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.
        endif.

        close dataset file_source. ""close file
        close dataset dst_source.  "close file
        clear ls_dir.
      endloop.

      create object o_json.
      o_json->lowercase_names      = abap_true.
      o_json->include_empty_values = abap_true.
      o_json->pretty_print         = abap_true.

      concatenate api_url '/interface/livclaim' into api_url.

      api_data = o_json->encode( livpo_returntovim ).
      call function 'Z_VIM_CALL_API'
        exporting
          api_data    = api_data
          api_url     = api_url
          api_method  = 'POST'
        importing
          lv_response = lv_response.
    endif.
  endmethod.                    "park_liv_claim

  method post_liv_claim.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'CLM2'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'POST_PO_CLAIM'.
        refresh : lt_liv_npo1, lt_liv_npo2.
        clear: file_source.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.
        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_liv_npo1.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_liv_npo1-invid
                                                ls_liv_npo1-lifnr
                                                ls_liv_npo1-bukrs
                                                ls_liv_npo1-bldat
                                                ls_liv_npo1-budat
                                                ls_liv_npo1-bupla
                                                ls_liv_npo1-xblnr
                                                ls_liv_npo1-sgtxt
                                                ls_liv_npo1-mwskz1
                                                ls_liv_npo1-rmwwr
                                                ls_liv_npo1-waers
                                                ls_liv_npo1-zfbdt
                                                ls_liv_npo1-zterm
                                                ls_liv_npo1-zlspr
                                                ls_liv_npo1-zuonr
                                                ls_liv_npo1-bktxt
                                                ls_liv_npo1-witht
                                                ls_liv_npo1-wt_withcd
                                                ls_liv_npo1-wt_amount
                                                ls_liv_npo1-hkont
                                                ls_liv_npo1-adjamount
                                                ls_liv_npo1-adtxcode
                                                ls_liv_npo1-adtext
                                                ls_liv_npo1-kostl
                                                ls_liv_npo1-sapdoc.

                    append ls_liv_npo1 to lt_liv_npo1.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_liv_npo1 is not initial.

        data :
          lt_glaccount type table of zsvim_livnpo_gl,
          ls_glaccount type zsvim_livnpo_gl,
          lt_wihtax    type table of zsvim_livnpo_wht,
          ls_wihtax    type zsvim_livnpo_wht,
          lt_return    type table of bdcmsgcoll,
          ls_return    type bdcmsgcoll.

        refresh : lt_glaccount, lt_wihtax, lt_return, lt_liv_npo2, lt_liv_npo3.

        append lines of lt_liv_npo1 to lt_liv_npo2.
        append lines of lt_liv_npo1 to lt_liv_npo3.

        sort lt_liv_npo1 by invid lifnr bukrs.
        sort lt_liv_npo2 by invid lifnr bukrs hkont adjamount adtxcode adtext bupla kostl.
        sort lt_liv_npo3 by invid lifnr bukrs witht.

        delete adjacent duplicates from lt_liv_npo1 comparing invid lifnr bukrs.
        delete adjacent duplicates from lt_liv_npo3 comparing invid lifnr bukrs witht.
        delete adjacent duplicates from lt_liv_npo2 comparing invid lifnr bukrs hkont adjamount adtxcode adtext bupla kostl.

        refresh lt_lfbw.
        select * from lfbw into table lt_lfbw
          for all entries in lt_liv_npo1[]
            where lifnr = lt_liv_npo1-lifnr and bukrs = lt_liv_npo1-bukrs.

        clear ls_liv_npo1.
        loop at lt_liv_npo1 into ls_liv_npo1.

          "Append GL Data
          clear : ls_liv_npo2, xtotaladj.
          loop at lt_liv_npo2 into ls_liv_npo2 where invid = ls_liv_npo1-invid and lifnr = ls_liv_npo1-lifnr.
            clear ls_glaccount.
            move-corresponding ls_liv_npo2 to ls_glaccount.
            append ls_glaccount to lt_glaccount.
            xtotaladj = xtotaladj + ls_glaccount-adjamount.
            clear ls_liv_npo2.
          endloop.

          "Witholding Tax data
          clear ls_liv_npo3.
          loop at lt_liv_npo3 into ls_liv_npo3 where invid = ls_liv_npo1-invid and lifnr = ls_liv_npo1-lifnr.
            clear ls_wihtax.
            move-corresponding ls_liv_npo3 to ls_wihtax.
            if ls_wihtax-witht is not initial.
              append ls_wihtax to lt_wihtax.
            endif.
            clear ls_liv_npo3.
          endloop.

          if lt_wihtax is initial.
            clear ls_lfbw.
            loop at lt_lfbw into ls_lfbw where lifnr = ls_liv_npo1-lifnr and bukrs = ls_liv_npo1-bukrs.
              clear ls_wihtax.
              ls_wihtax-witht     = ls_lfbw-witht.
              ls_wihtax-wt_withcd = ' '.
              append ls_wihtax to lt_wihtax.
              clear ls_lfbw.
            endloop.
          endif.

          clear ls_liv_npo1.
        endloop.

        clear ls_liv_npo1.
        read table lt_liv_npo1 into ls_liv_npo1 index 1.

        check ls_liv_npo1 is not initial.
        clear ls_liv_npo1-rmwwr.
        if ls_liv_npo1-waers = 'IDR'.
          ls_liv_npo1-rmwwr = round( val = xtotaladj dec = 0 mode = 5 ).
        else.
          ls_liv_npo1-rmwwr = xtotaladj.
        endif.
*        ls_liv_npo1-rmwwr = xtotaladj.
        condense ls_liv_npo1-rmwwr.
*        clear ls_liv_npo1-rmwwr.
*        ls_liv_npo1-rmwwr = round( val = xtotaladj dec = 0 mode = 5 ).
*        condense ls_liv_npo1-rmwwr.
        data v_kunnr type kna1-kunnr.
        clear v_kunnr.
        select single kunnr from lfa1 into v_kunnr where lifnr = ls_liv_npo1-lifnr.
        clear ls_liv_npo1-lifnr.
        ls_liv_npo1-lifnr = v_kunnr.
        clear v_kunnr.
        select single kunnr from knb1 into v_kunnr where kunnr = ls_liv_npo1-lifnr and bukrs = ls_liv_npo1-bukrs.

        if v_kunnr is not initial.
          clear ls_liv_npo1-lifnr.
          ls_liv_npo1-lifnr = v_kunnr.
          refresh lt_return.
          call function 'Z_VIM_POST_FV70'
            exporting
              i_header     = ls_liv_npo1
            tables
              lt_glaccount = lt_glaccount
              lt_wihtax    = lt_wihtax
              lt_return    = lt_return.

          clear ls_return.
          read table lt_return into ls_return with key msgtyp = 'E'.
          if sy-subrc <> 0.
            clear ls_return.
            read table lt_return into ls_return with key msgid = 'FP'.

            "Insert Log History
            clear ls_ztvim002.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            ls_ztvim002-transtype = 'CLM2'.
            ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '1'.
            ls_ztvim002-notes     = 'Invoice Claim Posted'.
            ls_ztvim002-refrence  = ls_liv_npo1-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.

            delete dataset file_source.
          else.
            "BAPI rollback
*          delete dataset file_source.
            data :
                msgno type symsgno.
            clear msgno.
            msgno = ls_return-msgnr.

            clear ls_ztvim002.
            call function 'FORMAT_MESSAGE'
              exporting
                id        = ls_return-msgid
                lang      = '-D'
                no        = msgno
                v1        = ls_return-msgv1
                v2        = ls_return-msgv2
                v3        = ls_return-msgv3
                v4        = ls_return-msgv4
              importing
                msg       = ls_ztvim002-notes
              exceptions
                not_found = 1
                others    = 2.

*            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
            if ls_ztvim002-transid is initial.
              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            endif.
            ls_ztvim002-transtype = 'CLM2'.
            ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '2'.
            ls_ztvim002-refrence  = ls_liv_npo1-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.

            delete dataset dst_source.
          endif.
        else.
          clear ls_ztvim002.
          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          ls_ztvim002-transtype = 'CLM2'.
          ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
          ls_ztvim002-notes     = 'Please maintain customer code for vendor'.
*            ls_ztvim002-refrence  = xawkey.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.
        endif.
        close dataset file_source. ""close file
        close dataset dst_source.  "close file
        clear ls_dir.
      endloop.

*      create object o_json.
*      o_json->lowercase_names      = abap_true.
*      o_json->include_empty_values = abap_true.
*      o_json->pretty_print         = abap_true.
*
*      concatenate api_url '/interface/livpo' into api_url.
*
*      api_data = o_json->encode( livpo_returntovim ).
*      call function 'Z_VIM_CALL_API'
*        exporting
*          api_data    = api_data
*          api_url     = api_url
*          api_method  = 'POST'
*        importing
*          lv_response = lv_response.
    endif.
  endmethod.                    "post_liv_claim

  method delt_liv_claim.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'CLM3'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'CNCL_PO_CLAIM'.
        refresh : lt_liv_npo1.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_liv_npo1.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_liv_npo1-invid
                                                ls_liv_npo1-lifnr
                                                ls_liv_npo1-sapdoc
                                                ls_liv_npo1-budat.

                    append ls_liv_npo1 to lt_liv_npo1.
                  endif.
                enddo.
              endif.

            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_liv_npo1 is not initial.

*lt_bdcmsg  type table of bdcmsgcoll,
*   ls_bdcmsg  type bdcmsgcoll,
        refresh lt_bdcmsg.
        clear ls_liv_npo1.
        loop at lt_liv_npo1 into ls_liv_npo1.
          call function 'Z_VIM_DELT_FV70'
            exporting
              i_header  = ls_liv_npo1
            tables
              lt_return = lt_bdcmsg.

          clear ls_bdcmsg.
          read table lt_bdcmsg into ls_bdcmsg with key msgtyp = 'E'.
          if sy-subrc <> 0.
            clear ls_ztvim002.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            ls_ztvim002-transtype = 'CLM3'.
            ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '1'.
            ls_ztvim002-notes     = 'Parked Document Deleted'.
            ls_ztvim002-refrence  = ls_liv_npo1-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.

            delete dataset file_source. ""delete file
          else.
            clear ls_ztvim002.
*            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
            if ls_ztvim002-transid is initial.
              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            endif.
            ls_ztvim002-transtype = 'CLM3'.
            ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '2'.
            ls_ztvim002-notes     = ls_bdcmsg-msgv1.
            ls_ztvim002-refrence  = ls_liv_npo1-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.
          endif.
          clear ls_liv_npo1.
        endloop.

        close dataset file_source. ""close file
        close dataset dst_source.  "close file
        clear ls_dir.
      endloop.
    endif.
  endmethod.                    "delt_liv_claim

  method revs_liv_claim.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'CLM4'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'REVS_PO_CLAIM'.
        refresh : lt_liv_npo1.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_liv_npo1.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_liv_npo1-invid
                                                ls_liv_npo1-lifnr
                                                ls_liv_npo1-sapdoc
                                                ls_liv_npo1-bukrs "Reason Reversal
                                                ls_liv_npo1-budat.

                    append ls_liv_npo1 to lt_liv_npo1.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_liv_npo1 is not initial.

        data :
              i_bukrs  type rbkp_v-bukrs,
              i_belnr  type rbkp_v-belnr,
              i_gjahr  type rbkp_v-gjahr,
              i_stgrd  type bkpf-stgrd,
              i_budat  type budat.

        data wa_reversal type bapiacrev.
        data wa_bkpf     type bkpf.
        data :
             obj_type	type bapiacrev-obj_type,
             obj_key  type bapiacrev-obj_key,
             obj_sys  type bapiacrev-obj_sys.

        clear ls_liv_npo1.
        read table lt_liv_npo1 into ls_liv_npo1 index 1.

        clear : i_bukrs, i_belnr, i_gjahr, i_stgrd, i_budat.

        i_bukrs = ls_liv_npo1-sapdoc+10(4).
        i_belnr = ls_liv_npo1-sapdoc(10).
        i_gjahr = ls_liv_npo1-sapdoc+14(4).
        i_stgrd = ls_liv_npo1-bukrs.
        i_budat = ls_liv_npo1-budat.

        refresh lt_bdc_return.
        call function 'Z_VIM_REVS_DOC'
          exporting
            i_belnr   = i_belnr
            i_bukrs   = i_bukrs
            i_gjahr   = i_gjahr
            i_stgrd   = i_stgrd
            i_budat   = i_budat
          tables
            lt_return = lt_bdc_return.


        clear ls_bdc_return.
        read table lt_bdc_return into ls_bdc_return with key msgtyp = 'E'.
        if sy-subrc = 0.
          clear ls_ztvim002.

          data :
                msgno type symsgno.
          clear msgno.
          msgno = ls_bdc_return-msgnr.

          clear ls_ztvim002.
          call function 'FORMAT_MESSAGE'
            exporting
              id        = ls_bdc_return-msgid
              lang      = '-D'
              no        = msgno
              v1        = ls_bdc_return-msgv1
              v2        = ls_bdc_return-msgv2
              v3        = ls_bdc_return-msgv3
              v4        = ls_bdc_return-msgv4
            importing
              msg       = ls_ztvim002-notes
            exceptions
              not_found = 1
              others    = 2.

          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'CLM4'.
          ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
          ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

        else.
          clear ls_bkpf.
          select single * from bkpf into ls_bkpf where bukrs = i_bukrs and belnr = i_belnr and gjahr = i_gjahr.
          if ls_bkpf-xreversal = '1' or ls_bkpf-stblg is not initial.
            clear livnpo_ls_ret.
            read table livnpo_lt_ret into livnpo_ls_ret with key type = 'S'.
            "Insert Log History
            clear ls_ztvim002.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            ls_ztvim002-transtype = 'CLM4'.
            ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '1'.
            ls_ztvim002-notes     = 'Invoice Claim Reversed'.
            ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.

            delete dataset file_source.
          else.
            clear ls_ztvim002.
            select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
            if ls_ztvim002-transid is initial.
              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            endif.
            ls_ztvim002-transtype = 'CLM4'.
            ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '2'.
            ls_ztvim002-notes     = 'Check reversal data'.
            ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.
          endif.
        endif.

*        call function 'CALL_FB08'
*          exporting
*            i_bukrs      = i_bukrs
*            i_belnr      = i_belnr
*            i_gjahr      = i_gjahr
*            i_stgrd      = i_stgrd
*            i_budat      = i_budat
**           I_MONAT      =
*          exceptions
*            not_possible = 1
*            others       = 2.
*
*        if sy-subrc <> 0.
*          "Insert Log History
*          clear ls_ztvim002.
*          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
*          if ls_ztvim002-transid is initial.
*            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
*          endif.
*          ls_ztvim002-transtype = 'CLM4'.
*          ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
*          ls_ztvim002-textfile  = ls_dir-name.
*          ls_ztvim002-status    = '2'.
*          ls_ztvim002-notes     = 'Document was already reversed or Document Not Found'.
*          ls_ztvim002-refrence  = ls_liv_npo1-sapdoc.
*          ls_ztvim002-erdat     = sy-datum.
*          ls_ztvim002-itime     = sy-uzeit.
*          ls_ztvim002-uname     = sy-uname.
*          modify ztvim002 from ls_ztvim002.
*        else.
*          call function 'BAPI_TRANSACTION_COMMIT'
*            exporting
*              wait = 'X'.
*
*          clear livnpo_ls_ret.
*          read table livnpo_lt_ret into livnpo_ls_ret with key type = 'S'.
*          "Insert Log History
*          clear ls_ztvim002.
*          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
*          ls_ztvim002-transtype = 'CLM4'.
*          ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
*          ls_ztvim002-textfile  = ls_dir-name.
*          ls_ztvim002-status    = '1'.
*          ls_ztvim002-notes     = 'Invoice Claim Reversed'.
*          ls_ztvim002-refrence  = ls_liv_npo1-sapdoc.
*          ls_ztvim002-erdat     = sy-datum.
*          ls_ztvim002-itime     = sy-uzeit.
*          ls_ztvim002-uname     = sy-uname.
*          modify ztvim002 from ls_ztvim002.
*
*          delete dataset file_source.
*        endif.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.
    endif.
  endmethod.                    "revs_liv_claim


  method park_liv_npo.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'IVNPO1'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear   : livpo_returntovim, ls_dir.
      refresh : livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'PARK_NPO'.
        refresh : lt_liv_npo1, lt_liv_npo2.
        clear: file_source.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.
        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_liv_npo1.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_liv_npo1-invid
                                                ls_liv_npo1-lifnr
                                                ls_liv_npo1-bukrs
                                                ls_liv_npo1-bldat
                                                ls_liv_npo1-budat
                                                ls_liv_npo1-bupla
                                                ls_liv_npo1-xblnr
                                                ls_liv_npo1-sgtxt
                                                ls_liv_npo1-mwskz1
                                                ls_liv_npo1-rmwwr
                                                ls_liv_npo1-waers
                                                ls_liv_npo1-zfbdt
                                                ls_liv_npo1-zterm
                                                ls_liv_npo1-zlspr
                                                ls_liv_npo1-zuonr
                                                ls_liv_npo1-bktxt
                                                ls_liv_npo1-witht
                                                ls_liv_npo1-wt_withcd
                                                ls_liv_npo1-wt_amount
                                                ls_liv_npo1-hkont
                                                ls_liv_npo1-adjamount
                                                ls_liv_npo1-adtxcode
                                                ls_liv_npo1-adtext
                                                ls_liv_npo1-kostl
                                                ls_liv_npo1-sapdoc
                                                ls_liv_npo1-shkzg.

                    append ls_liv_npo1 to lt_liv_npo1.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_liv_npo1 is not initial.

        data :
          lt_glaccount type table of zsvim_livnpo_gl,
          ls_glaccount type zsvim_livnpo_gl,
          lt_wihtax    type table of zsvim_livnpo_wht,
          ls_wihtax    type zsvim_livnpo_wht,
          lt_return    type table of bdcmsgcoll,
          ls_return    type bdcmsgcoll.

        refresh : lt_glaccount, lt_wihtax, lt_return, lt_liv_npo2, lt_liv_npo3.

        append lines of lt_liv_npo1 to lt_liv_npo2.
        append lines of lt_liv_npo1 to lt_liv_npo3.

        sort lt_liv_npo1 by invid lifnr bukrs.
        sort lt_liv_npo2 by invid lifnr bukrs hkont adjamount adtxcode adtext bupla kostl.
        sort lt_liv_npo3 by invid lifnr bukrs witht.

        delete adjacent duplicates from lt_liv_npo1 comparing invid lifnr bukrs.
        delete adjacent duplicates from lt_liv_npo3 comparing invid lifnr bukrs witht.
        delete adjacent duplicates from lt_liv_npo2 comparing invid lifnr bukrs hkont adjamount adtxcode adtext bupla kostl.

        refresh lt_lfbw.
        select * from lfbw into table lt_lfbw
          for all entries in lt_liv_npo1[]
            where lifnr = lt_liv_npo1-lifnr and bukrs = lt_liv_npo1-bukrs.

        clear : ls_liv_npo1, xtotaladj.
        loop at lt_liv_npo1 into ls_liv_npo1.

          "Append GL Data
          clear ls_liv_npo2.
          loop at lt_liv_npo2 into ls_liv_npo2 where invid = ls_liv_npo1-invid and lifnr = ls_liv_npo1-lifnr.
            clear ls_glaccount.
            move-corresponding ls_liv_npo2 to ls_glaccount.
            append ls_glaccount to lt_glaccount.
            xtotaladj = xtotaladj + ls_glaccount-adjamount.
            clear ls_liv_npo2.
          endloop.

          "Witholding Tax data
          clear ls_liv_npo3.
          loop at lt_liv_npo3 into ls_liv_npo3 where invid = ls_liv_npo1-invid and lifnr = ls_liv_npo1-lifnr.
            clear ls_wihtax.
            move-corresponding ls_liv_npo3 to ls_wihtax.
            if ls_wihtax-witht is not initial.
              append ls_wihtax to lt_wihtax.
            endif.
            clear ls_liv_npo3.
          endloop.

          if lt_wihtax is initial.
            clear ls_lfbw.
            loop at lt_lfbw into ls_lfbw where lifnr = ls_liv_npo1-lifnr and bukrs = ls_liv_npo1-bukrs.
              clear ls_wihtax.
              ls_wihtax-witht     = ls_lfbw-witht.
              ls_wihtax-wt_withcd = ' '.
              append ls_wihtax to lt_wihtax.
              clear ls_lfbw.
            endloop.
          endif.

          clear ls_liv_npo1.
        endloop.

        clear ls_liv_npo1.
        read table lt_liv_npo1 into ls_liv_npo1 index 1.

        check ls_liv_npo1 is not initial.
        clear ls_liv_npo1-rmwwr.
        if ls_liv_npo1-waers = 'IDR'.
          ls_liv_npo1-rmwwr = round( val = xtotaladj dec = 0 mode = 5 ).
        else.
          ls_liv_npo1-rmwwr = xtotaladj.
        endif.
*        ls_liv_npo1-rmwwr = round( val = xtotaladj dec = 0 mode = 5 ).
*        ls_liv_npo1-rmwwr = xtotaladj.
        condense ls_liv_npo1-rmwwr.

        refresh lt_return.
        call function 'Z_VIM_PARK_FV60'
          exporting
            i_header     = ls_liv_npo1
          tables
            lt_glaccount = lt_glaccount
            lt_wihtax    = lt_wihtax
            lt_return    = lt_return.

        wait up to 1 seconds.

        clear ls_return.
        read table lt_return into ls_return with key msgtyp = 'E'.
        if sy-subrc <> 0.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.

          delete lt_return where msgid <> 'FP'.
          delete lt_return where msgv1 = ''.

          clear ls_return.
          read table lt_return into ls_return with key msgid = 'FP'.

          if ls_return-msgv1 is initial.
            continue.
          endif.

          clear xawkey.
          select single awkey from bkpf into xawkey
            where bukrs = ls_liv_npo1-bukrs and belnr = ls_return-msgv1 and gjahr = ls_liv_npo1-budat(4).

*          concatenate ls_return-msgv1 ls_liv_npo1-bukrs ls_liv_npo1-budat(4) into xawkey.

          "Insert Log History
          clear ls_ztvim002.
          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          ls_ztvim002-transtype = 'IVNPO1'.
          ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = 'Invoice Parked'.
          ls_ztvim002-refrence  = xawkey.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.


          clear ls_interface01x.
          ls_interface01x-invid  = ls_liv_npo1-invid.
          ls_interface01x-sapdoc = xawkey.
          append ls_interface01x to livpo_returntovim-vim_iv[].

          submit zvim004
            with _invid  = ls_liv_npo1-invid
            with _sapdoc = xawkey
            with _bukrs  = ls_liv_npo1-bukrs
            with _belnr  = ls_return-msgv1
            with _gjahr  = ls_liv_npo1-budat(4)
            with _opt2   = ' '
            with _opt4   = ' '
            with _opt3   = 'X'
            with rd_ap1a = 'X' and return.

          delete dataset file_source.
        else.
          "BAPI rollback
*          call function 'BAPI_TRANSACTION_ROLLBACK'.

          data :
              msgno type symsgno.
          clear msgno.
          msgno = ls_return-msgnr.

          clear ls_ztvim002.
          call function 'FORMAT_MESSAGE'
            exporting
              id        = ls_return-msgid
              lang      = '-D'
              no        = msgno
              v1        = ls_return-msgv1
              v2        = ls_return-msgv2
              v3        = ls_return-msgv3
              v4        = ls_return-msgv4
            importing
              msg       = ls_ztvim002-notes
            exceptions
              not_found = 1
              others    = 2.

*          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'IVNPO1'.
          ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
*          ls_ztvim002-notes     = ls_return-msgv1.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

          delete dataset dst_source.
        endif.

        close dataset file_source. ""close file
        close dataset dst_source.  "close file
        clear ls_dir.
      endloop.

      create object o_json.
      o_json->lowercase_names      = abap_true.
      o_json->include_empty_values = abap_true.
      o_json->pretty_print         = abap_true.

      concatenate api_url '/interface/livnonpo' into api_url.

      api_data = o_json->encode( livpo_returntovim ).
      call function 'Z_VIM_CALL_API'
        exporting
          api_data    = api_data
          api_url     = api_url
          api_method  = 'POST'
        importing
          lv_response = lv_response.
    endif.
  endmethod.                    "readfileinvnpo

  method post_liv_npo.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'IVNPO2'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'POST_NPO'.
        refresh : lt_liv_npo1, lt_liv_npo2.
        clear: file_source.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.
        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_liv_npo1.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_liv_npo1-invid
                                                ls_liv_npo1-lifnr
                                                ls_liv_npo1-bukrs
                                                ls_liv_npo1-bldat
                                                ls_liv_npo1-budat
                                                ls_liv_npo1-bupla
                                                ls_liv_npo1-xblnr
                                                ls_liv_npo1-sgtxt
                                                ls_liv_npo1-mwskz1
                                                ls_liv_npo1-rmwwr
                                                ls_liv_npo1-waers
                                                ls_liv_npo1-zfbdt
                                                ls_liv_npo1-zterm
                                                ls_liv_npo1-zlspr
                                                ls_liv_npo1-zuonr
                                                ls_liv_npo1-bktxt
                                                ls_liv_npo1-witht
                                                ls_liv_npo1-wt_withcd
                                                ls_liv_npo1-wt_amount
                                                ls_liv_npo1-hkont
                                                ls_liv_npo1-adjamount
                                                ls_liv_npo1-adtxcode
                                                ls_liv_npo1-adtext
                                                ls_liv_npo1-kostl
                                                ls_liv_npo1-sapdoc.

                    append ls_liv_npo1 to lt_liv_npo1.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_liv_npo1 is not initial.

        data :
          lt_glaccount type table of zsvim_livnpo_gl,
          ls_glaccount type zsvim_livnpo_gl,
          lt_wihtax    type table of zsvim_livnpo_wht,
          ls_wihtax    type zsvim_livnpo_wht,
          lt_return    type table of bdcmsgcoll,
          ls_return    type bdcmsgcoll.

        refresh : lt_glaccount, lt_wihtax, lt_return, lt_liv_npo2, lt_liv_npo3.

        append lines of lt_liv_npo1 to lt_liv_npo2.
        append lines of lt_liv_npo1 to lt_liv_npo3.

        sort lt_liv_npo1 by invid lifnr bukrs.
        sort lt_liv_npo2 by invid lifnr bukrs hkont adjamount adtxcode adtext bupla kostl.
        sort lt_liv_npo3 by invid lifnr bukrs witht.

        delete adjacent duplicates from lt_liv_npo1 comparing invid lifnr bukrs.
        delete adjacent duplicates from lt_liv_npo3 comparing invid lifnr bukrs witht.
        delete adjacent duplicates from lt_liv_npo2 comparing invid lifnr bukrs hkont adjamount adtxcode adtext bupla kostl.

        refresh lt_lfbw.
        select * from lfbw into table lt_lfbw
          for all entries in lt_liv_npo1[]
            where lifnr = lt_liv_npo1-lifnr and bukrs = lt_liv_npo1-bukrs.

        clear ls_liv_npo1.
        loop at lt_liv_npo1 into ls_liv_npo1.

          "Append GL Data
          clear : ls_liv_npo2, xtotaladj.
          loop at lt_liv_npo2 into ls_liv_npo2 where invid = ls_liv_npo1-invid and lifnr = ls_liv_npo1-lifnr.
            clear ls_glaccount.
            move-corresponding ls_liv_npo2 to ls_glaccount.
            append ls_glaccount to lt_glaccount.
            xtotaladj = xtotaladj + ls_glaccount-adjamount.
            clear ls_liv_npo2.
          endloop.

          "Witholding Tax data
          clear ls_liv_npo3.
          loop at lt_liv_npo3 into ls_liv_npo3 where invid = ls_liv_npo1-invid and lifnr = ls_liv_npo1-lifnr.
            clear ls_wihtax.
            move-corresponding ls_liv_npo3 to ls_wihtax.
            if ls_wihtax-witht is not initial.
              append ls_wihtax to lt_wihtax.
            endif.
            clear ls_liv_npo3.
          endloop.

          if lt_wihtax is initial.
            clear ls_lfbw.
            loop at lt_lfbw into ls_lfbw where lifnr = ls_liv_npo1-lifnr and bukrs = ls_liv_npo1-bukrs.
              clear ls_wihtax.
              ls_wihtax-witht     = ls_lfbw-witht.
              ls_wihtax-wt_withcd = ' '.
              append ls_wihtax to lt_wihtax.
              clear ls_lfbw.
            endloop.
          endif.

          clear ls_liv_npo1.
        endloop.

        clear ls_liv_npo1.
        read table lt_liv_npo1 into ls_liv_npo1 index 1.

        check ls_liv_npo1 is not initial.
        clear ls_liv_npo1-rmwwr.
*        ls_liv_npo1-rmwwr = xtotaladj.
        if ls_liv_npo1-waers = 'IDR'.
          ls_liv_npo1-rmwwr = round( val = xtotaladj dec = 0 mode = 5 ).
        else.
          ls_liv_npo1-rmwwr = xtotaladj.
        endif.
        condense ls_liv_npo1-rmwwr.
*        clear ls_liv_npo1-rmwwr.
*        ls_liv_npo1-rmwwr = round( val = xtotaladj dec = 0 mode = 5 ).
*        condense ls_liv_npo1-rmwwr.

        call function 'Z_VIM_POST_FV60'
          exporting
            i_header     = ls_liv_npo1
          tables
            lt_glaccount = lt_glaccount
            lt_wihtax    = lt_wihtax
            lt_return    = lt_return.

        wait up to 1 seconds.

        clear ls_return.
        read table lt_return into ls_return with key msgtyp = 'E'.
        if sy-subrc <> 0.

          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.

          clear ls_bkpf.
          select single * from bkpf into ls_bkpf
            where awkey = ls_liv_npo1-sapdoc.

          if ls_bkpf-bstat <> 'V'.
            clear ls_return.
            read table lt_return into ls_return with key msgid = 'FP'.

            "Insert Log History
            clear ls_ztvim002.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            ls_ztvim002-transtype = 'IVNPO2'.
            ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '1'.
            ls_ztvim002-notes     = 'Invoice Posted'.
            ls_ztvim002-refrence  = ls_liv_npo1-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.

            delete dataset file_source.
          endif.
        else.
          "BAPI rollback
*          call function 'BAPI_TRANSACTION_ROLLBACK'.
          data :
              msgno type symsgno.
          clear msgno.
          msgno = ls_return-msgnr.

          clear ls_ztvim002.
          call function 'FORMAT_MESSAGE'
            exporting
              id        = ls_return-msgid
              lang      = '-D'
              no        = msgno
              v1        = ls_return-msgv1
              v2        = ls_return-msgv2
              v3        = ls_return-msgv3
              v4        = ls_return-msgv4
            importing
              msg       = ls_ztvim002-notes
            exceptions
              not_found = 1
              others    = 2.

*          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'IVNPO2'.
          ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
          ls_ztvim002-refrence  = ls_liv_npo1-sapdoc.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.
          delete dataset dst_source.
        endif.

        close dataset file_source. ""close file
        close dataset dst_source.  "close file
        clear ls_dir.
      endloop.

*      create object o_json.
*      o_json->lowercase_names      = abap_true.
*      o_json->include_empty_values = abap_true.
*      o_json->pretty_print         = abap_true.
*
*      concatenate api_url '/interface/livpo' into api_url.
*
*      api_data = o_json->encode( livpo_returntovim ).
*      call function 'Z_VIM_CALL_API'
*        exporting
*          api_data    = api_data
*          api_url     = api_url
*          api_method  = 'POST'
*        importing
*          lv_response = lv_response.
    endif.
  endmethod.                    "post_liv_npo

  method delt_liv_npo.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'IVNPO3'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'CNCL_NPO'.
        refresh : lt_liv_npo1.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_liv_npo1.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_liv_npo1-invid
                                                ls_liv_npo1-lifnr
                                                ls_liv_npo1-sapdoc
                                                ls_liv_npo1-budat.

                    append ls_liv_npo1 to lt_liv_npo1.
                  endif.
                enddo.
              endif.

            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_liv_npo1 is not initial.

        refresh lt_bdcmsg.
        clear ls_liv_npo1.
        loop at lt_liv_npo1 into ls_liv_npo1.
          call function 'Z_VIM_DELT_FV60'
            exporting
              i_header  = ls_liv_npo1
            tables
              lt_return = lt_bdcmsg.

          wait up to 1 seconds.

          clear ls_bdcmsg.
          read table lt_bdcmsg into ls_bdcmsg with key msgtyp = 'E'.
          if sy-subrc <> 0.
            clear ls_ztvim002.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            ls_ztvim002-transtype = 'IVNPO3'.
            ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '1'.
            ls_ztvim002-notes     = 'Parked Document Deleted'.
            ls_ztvim002-refrence  = ls_liv_npo1-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.

            call function 'BAPI_TRANSACTION_COMMIT'
              exporting
                wait = 'X'.

            delete dataset file_source. ""delete file
          else.
            clear ls_ztvim002.
*            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
            if ls_ztvim002-transid is initial.
              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            endif.
            ls_ztvim002-transtype = 'IVNPO3'.
            ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '2'.
            ls_ztvim002-notes     = ls_bdcmsg-msgv1.
            ls_ztvim002-refrence  = ls_liv_npo1-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.
          endif.
          clear ls_liv_npo1.
        endloop.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.
    endif.
  endmethod.                    "delt_liv_npo

  method revs_liv_npo.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'IVNPO4'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'REVS_NPO'.
        refresh : lt_liv_npo1.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_liv_npo1.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_liv_npo1-invid
                                                ls_liv_npo1-lifnr
                                                ls_liv_npo1-sapdoc
                                                ls_liv_npo1-bukrs "Reason Reversal
                                                ls_liv_npo1-budat.

                    append ls_liv_npo1 to lt_liv_npo1.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_liv_npo1 is not initial.

        data :
              i_bukrs  type rbkp_v-bukrs,
              i_belnr  type rbkp_v-belnr,
              i_gjahr  type rbkp_v-gjahr,
              i_stgrd  type bkpf-stgrd,
              i_budat  type budat.

        data wa_reversal type bapiacrev.
        data wa_bkpf     type bkpf.
        data :
             obj_type	type bapiacrev-obj_type,
             obj_key  type bapiacrev-obj_key,
             obj_sys  type bapiacrev-obj_sys.

        clear ls_liv_npo1.
        read table lt_liv_npo1 into ls_liv_npo1 index 1.

        clear : i_bukrs, i_belnr, i_gjahr, i_stgrd, i_budat.

        i_bukrs = ls_liv_npo1-sapdoc+10(4).
        i_belnr = ls_liv_npo1-sapdoc(10).
        i_gjahr = ls_liv_npo1-sapdoc+14(4).
        i_stgrd = ls_liv_npo1-bukrs.
        i_budat = ls_liv_npo1-budat.

        refresh lt_bdc_return.
        call function 'Z_VIM_REVS_DOC'
          exporting
            i_belnr   = i_belnr
            i_bukrs   = i_bukrs
            i_gjahr   = i_gjahr
            i_stgrd   = i_stgrd
            i_budat   = i_budat
          tables
            lt_return = lt_bdc_return.


        clear ls_bdc_return.
        read table lt_bdc_return into ls_bdc_return with key msgtyp = 'E'.
        if sy-subrc = 0.
          clear ls_ztvim002.

          data :
                msgno type symsgno.
          clear msgno.
          msgno = ls_bdc_return-msgnr.

          clear ls_ztvim002.
          call function 'FORMAT_MESSAGE'
            exporting
              id        = ls_bdc_return-msgid
              lang      = '-D'
              no        = msgno
              v1        = ls_bdc_return-msgv1
              v2        = ls_bdc_return-msgv2
              v3        = ls_bdc_return-msgv3
              v4        = ls_bdc_return-msgv4
            importing
              msg       = ls_ztvim002-notes
            exceptions
              not_found = 1
              others    = 2.

          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'IVNPO4'.
          ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
          ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

        else.
          clear ls_bkpf.
          select single * from bkpf into ls_bkpf where bukrs = i_bukrs and belnr = i_belnr and gjahr = i_gjahr.
          if ls_bkpf-xreversal = '1' or ls_bkpf-stblg is not initial.
            clear livnpo_ls_ret.
            read table livnpo_lt_ret into livnpo_ls_ret with key type = 'S'.
            "Insert Log History
            clear ls_ztvim002.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            ls_ztvim002-transtype = 'IVNPO4'.
            ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '1'.
            ls_ztvim002-notes     = 'Invoice Non PO Reversed'.
            ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.

            delete dataset file_source.
          else.
            clear ls_ztvim002.
            select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
            if ls_ztvim002-transid is initial.
              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            endif.
            ls_ztvim002-transtype = 'IVNPO4'.
            ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '2'.
            ls_ztvim002-notes     = 'Check reversal data'.
            ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.
          endif.
        endif.

*        call function 'CALL_FB08'
*          exporting
*            i_bukrs      = i_bukrs
*            i_belnr      = i_belnr
*            i_gjahr      = i_gjahr
*            i_stgrd      = i_stgrd
*            i_budat      = i_budat
**           I_MONAT      =
*          exceptions
*            not_possible = 1
*            others       = 2.
*
*        wait up to 1 seconds.
*
*        if sy-subrc <> 0.
*          "Insert Log History
*          clear ls_ztvim002.
*          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
*          if ls_ztvim002-transid is initial.
*            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
*          endif.
*          ls_ztvim002-transtype = 'IVNPO4'.
*          ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
*          ls_ztvim002-textfile  = ls_dir-name.
*          ls_ztvim002-status    = '2'.
*          ls_ztvim002-notes     = 'Document was already reversed or Document Not Found'.
*          ls_ztvim002-refrence  = ls_liv_npo1-sapdoc.
*          ls_ztvim002-erdat     = sy-datum.
*          ls_ztvim002-itime     = sy-uzeit.
*          ls_ztvim002-uname     = sy-uname.
*          modify ztvim002 from ls_ztvim002.
*        else.
*          call function 'BAPI_TRANSACTION_COMMIT'
*            exporting
*              wait = 'X'.
*
*          clear livnpo_ls_ret.
*          read table livnpo_lt_ret into livnpo_ls_ret with key type = 'S'.
*          "Insert Log History
*          clear ls_ztvim002.
*          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
*          ls_ztvim002-transtype = 'IVNPO4'.
*          ls_ztvim002-lifnr     = ls_liv_npo1-lifnr.
*          ls_ztvim002-textfile  = ls_dir-name.
*          ls_ztvim002-status    = '1'.
*          ls_ztvim002-notes     = 'Invoice Reversed'.
*          ls_ztvim002-refrence  = ls_liv_npo1-sapdoc.
*          ls_ztvim002-erdat     = sy-datum.
*          ls_ztvim002-itime     = sy-uzeit.
*          ls_ztvim002-uname     = sy-uname.
*          modify ztvim002 from ls_ztvim002.
*
*          delete dataset file_source.
*        endif.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.
    endif.
  endmethod.                    "revs_liv_npo

  method park_dp_po.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'DPM1'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear : ls_dir, livpo_returntovim.

      refresh livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'PARK_DP_PO'.
        refresh : lt_dp_po01.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_dp_po01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab
                      into ls_dp_po01-dpvid
                           ls_dp_po01-lifnr
                           ls_dp_po01-bldat
                           ls_dp_po01-budat
                           ls_dp_po01-bukrs
                           ls_dp_po01-waers
                           ls_dp_po01-xblnr
                           ls_dp_po01-bktxt
                           ls_dp_po01-bupla
                           ls_dp_po01-ebeln
                           ls_dp_po01-ebelp
                           ls_dp_po01-netwr
                           ls_dp_po01-mwskz
                           ls_dp_po01-zfbdt
                           ls_dp_po01-zuonr
                           ls_dp_po01-sgtxt.

                    call function 'CONVERSION_EXIT_ALPHA_INPUT'
                      exporting
                        input  = ls_dp_po01-ebelp
                      importing
                        output = ls_dp_po01-ebelp.

                    append ls_dp_po01 to lt_dp_po01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_dp_po01 is not initial.

        clear ls_dp_po01.
        read table lt_dp_po01 into ls_dp_po01 index  1.

        refresh lt_acchd.
        clear ls_acchd.
        ls_acchd-awtyp = 'BKPFF'.
        ls_acchd-glvor = 'RFST'.
        ls_acchd-usnam = sy-uname.
        ls_acchd-tcode = 'FBA6'.
        ls_acchd-cpudt = sy-datum.
        ls_acchd-cputm = sy-uzeit.
        ls_acchd-bktxt = ls_dp_po01-bktxt.
        append ls_acchd to lt_acchd.

        refresh lt_accit.
        clear ls_accit.
        ls_accit-posnr = 1.
        ls_accit-bukrs = ls_dp_po01-bukrs.
        ls_accit-gjahr = ls_dp_po01-budat(4).
        ls_accit-bldat = ls_dp_po01-bldat.
        ls_accit-budat = ls_dp_po01-budat.
        ls_accit-wwert = ls_dp_po01-budat.
        ls_accit-monat = ls_dp_po01-budat+4(2).
        ls_accit-blart = 'KA'.
        ls_accit-xblnr = ls_dp_po01-xblnr.
        ls_accit-vorgn = 'AZAF'.
        ls_accit-mwskz = ls_dp_po01-mwskz.
        ls_accit-xmwst = 'X'.
        ls_accit-bupla = ls_dp_po01-bupla.
*        ls_accit-bstat = 'A'.
        ls_accit-zumsk = 'A'.
        ls_accit-umskz = 'F'.
        ls_accit-bstat = 'S'.
        ls_accit-bschl = '39'.
        ls_accit-shkzg = 'H'.
        ls_accit-koart = 'K'.
        ls_accit-lifnr = ls_dp_po01-lifnr.
        ls_accit-zfbdt = ls_dp_po01-zfbdt.
        ls_accit-zuonr = ls_dp_po01-zuonr.
        ls_accit-sgtxt = ls_dp_po01-sgtxt.
        ls_accit-gsber = ls_dp_po01-bupla.
        ls_accit-ebeln = ls_dp_po01-ebeln.
        ls_accit-ebelp = ls_dp_po01-ebelp.
        ls_accit-vatdate = sy-datum.
        append ls_accit to lt_accit.

        clear ls_a003.
        select single * from a003 into ls_a003
          where kappl = 'TX' and kschl = 'MWVS' and aland = 'ID' and mwskz = ls_dp_po01-mwskz.

        clear ls_konp.
        select single kbetr konwa from konp into (ls_konp-kbetr, ls_konp-konwa)
          where knumh = ls_a003-knumh.

        ls_konp-kbetr = ( ls_konp-kbetr / 100 ) * 10.

*        if ls_dp_po01-waers  = 'IDR'.
*          ls_dp_po01-netwr = ls_dp_po01-netwr / 100.
*        endif.

        ls_dp_po01-netwr = ls_dp_po01-netwr / 100.

        refresh lt_acccr.
        clear ls_acccr.
        ls_acccr-awtyp = 'BKPFF'.
        ls_acccr-posnr = 1.
        ls_acccr-curtp = '00'.
        ls_acccr-waers = ls_dp_po01-waers.
        ls_acccr-wrbtr = ls_dp_po01-netwr * -1.
        ls_acccr-wmwst = ( ls_dp_po01-netwr * ( ls_konp-kbetr / 100 ) ).
        ls_acccr-wmwst = ls_acccr-wmwst * -1.
        append ls_acccr to lt_acccr.

        clear ls_acccr.
        ls_acccr-awtyp = 'BKPFF'.
        ls_acccr-posnr = 1.
        ls_acccr-curtp = '10'.
        ls_acccr-waers = ls_dp_po01-waers.
        ls_acccr-wrbtr = ls_dp_po01-netwr * -1.
        ls_acccr-wmwst = ( ls_dp_po01-netwr * ( ls_konp-kbetr / 100 ) ).
        ls_acccr-wmwst = ls_acccr-wmwst * -1.
        append ls_acccr to lt_acccr.

        clear : e_bukrs, e_belnr, e_gjahr.
        refresh lt_return.
        call function 'Z_VIM_DP_PO'
          exporting
            i_test   = ' '
          importing
            e_bukrs  = e_bukrs
            e_belnr  = e_belnr
            e_gjahr  = e_gjahr
          tables
            t_acchd  = lt_acchd
            t_accit  = lt_accit
            t_acccr  = lt_acccr
            t_acctx  = lt_acctx
            t_return = lt_return.

        if e_belnr is not initial and e_bukrs is not initial and e_gjahr is not initial.
          clear xawkey.
          concatenate e_belnr e_bukrs into xawkey.
          concatenate xawkey ' ' e_gjahr into xawkey separated by space.
*          select single awkey from bkpf into xawkey
*            where bukrs = e_bukrs and belnr = e_belnr and gjahr = e_gjahr.
          "Insert Log History
          clear ls_ztvim002.
          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          ls_ztvim002-transtype = 'DPM1'.
          ls_ztvim002-lifnr     = ls_dp_po01-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = 'Down Payment Created'.
          ls_ztvim002-refrence  = xawkey.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

          clear ls_interface01x.
          ls_interface01x-invid  = ls_dp_po01-dpvid.
          ls_interface01x-sapdoc = xawkey.
          append ls_interface01x to livpo_returntovim-vim_iv[].

          submit zvim004
            with _invid  = ls_dp_po01-dpvid
            with _sapdoc = xawkey
            with _bukrs  = e_bukrs
            with _belnr  = e_belnr
            with _gjahr  = e_gjahr
            with _opt2   = ' '
            with _opt4   = ' '
            with _opt3   = 'X'
            with rd_ap1a = 'X' and return.

          delete dataset file_source.
        else.
          clear ls_return.
          read table lt_return into ls_return with key msgty = 'E'.

          data :
                msgno type symsgno.
          clear msgno.
          msgno = ls_return-msgno.

          clear ls_ztvim002.
          call function 'FORMAT_MESSAGE'
            exporting
              id        = ls_return-msgid
              lang      = '-D'
              no        = msgno
              v1        = ls_return-msgv1
              v2        = ls_return-msgv2
              v3        = ls_return-msgv3
              v4        = ls_return-msgv4
            importing
              msg       = ls_ztvim002-notes
            exceptions
              not_found = 1
              others    = 2.

          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.

          ls_ztvim002-transtype = 'DPM1'.
          ls_ztvim002-lifnr     = ls_dp_po01-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
*          concatenate ls_return-msgv1 ls_return-msgv2 ls_return-msgv3 ls_return-msgv4 into ls_ztvim002-notes separated by space.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.
        endif.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.

      if livpo_returntovim is not initial.
        create object o_json.
        o_json->lowercase_names      = abap_true.
        o_json->include_empty_values = abap_true.
        o_json->pretty_print         = abap_true.

        concatenate api_url '/interface/dppo' into api_url.
        clear lv_response.
        api_data = o_json->encode( livpo_returntovim ).
        call function 'Z_VIM_CALL_API'
          exporting
            api_data    = api_data
            api_url     = api_url
            api_method  = 'POST'
          importing
            lv_response = lv_response.
      endif.
    endif.
  endmethod.                    "readfiledp

  method revs_dp_po.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'DPM4'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'REVS_DP_PO'.
        refresh : lt_dp_npo01.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_dp_npo01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_dp_npo01-dpvid
                                                ls_dp_npo01-lifnr
                                                ls_dp_npo01-sapdoc
                                                ls_dp_npo01-bukrs "Reason Reversal
                                                ls_dp_npo01-budat.

                    append ls_dp_npo01 to lt_dp_npo01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_dp_npo01 is not initial.

        data :
              i_bukrs  type rbkp_v-bukrs,
              i_belnr  type rbkp_v-belnr,
              i_gjahr  type rbkp_v-gjahr,
              i_stgrd  type bkpf-stgrd,
              i_budat  type budat.

        data wa_reversal type bapiacrev.
        data wa_bkpf     type bkpf.
        data :
             obj_type	type bapiacrev-obj_type,
             obj_key  type bapiacrev-obj_key,
             obj_sys  type bapiacrev-obj_sys.

        clear ls_dp_npo01.
        read table lt_dp_npo01 into ls_dp_npo01 index 1.

        clear : i_bukrs, i_belnr, i_gjahr, i_stgrd, i_budat.

        i_bukrs = ls_dp_npo01-sapdoc+10(4).
        i_belnr = ls_dp_npo01-sapdoc(10).
        i_gjahr = ls_dp_npo01-sapdoc+14(4).
        i_stgrd = ls_dp_npo01-bukrs.
        i_budat = ls_dp_npo01-budat.

        refresh lt_bdc_return.
        call function 'Z_VIM_REVS_DOC'
          exporting
            i_belnr   = i_belnr
            i_bukrs   = i_bukrs
            i_gjahr   = i_gjahr
            i_stgrd   = i_stgrd
            i_budat   = i_budat
          tables
            lt_return = lt_bdc_return.


        clear ls_bdc_return.
        read table lt_bdc_return into ls_bdc_return with key msgtyp = 'E'.
        if sy-subrc = 0.
          clear ls_ztvim002.

          data :
                msgno type symsgno.
          clear msgno.
          msgno = ls_bdc_return-msgnr.

          clear ls_ztvim002.
          call function 'FORMAT_MESSAGE'
            exporting
              id        = ls_bdc_return-msgid
              lang      = '-D'
              no        = msgno
              v1        = ls_bdc_return-msgv1
              v2        = ls_bdc_return-msgv2
              v3        = ls_bdc_return-msgv3
              v4        = ls_bdc_return-msgv4
            importing
              msg       = ls_ztvim002-notes
            exceptions
              not_found = 1
              others    = 2.

          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'DPM4'.
          ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
          ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

        else.
          clear ls_bkpf.
          select single * from bkpf into ls_bkpf where bukrs = i_bukrs and belnr = i_belnr and gjahr = i_gjahr.
          if ls_bkpf-xreversal = '1' or ls_bkpf-stblg is not initial.
            clear livnpo_ls_ret.
            read table livnpo_lt_ret into livnpo_ls_ret with key type = 'S'.
            "Insert Log History
            clear ls_ztvim002.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            ls_ztvim002-transtype = 'DPM4'.
            ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '1'.
            ls_ztvim002-notes     = 'Down Payment Reversed'.
            ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.

            delete dataset file_source.
          else.
            clear ls_ztvim002.
            select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
            if ls_ztvim002-transid is initial.
              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            endif.
            ls_ztvim002-transtype = 'DPM4'.
            ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '2'.
            ls_ztvim002-notes     = 'Check reversal data'.
            ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.
          endif.
        endif.


*        call function 'CALL_FB08'
*          exporting
*            i_bukrs      = i_bukrs
*            i_belnr      = i_belnr
*            i_gjahr      = i_gjahr
*            i_stgrd      = i_stgrd
*            i_budat      = i_budat
**           I_MONAT      =
*          exceptions
*            not_possible = 1
*            others       = 2.
*
*        wait up to 1 seconds.
*
*        if sy-subrc <> 0.
**          call function 'BAPI_TRANSACTION_ROLLBACK'.
*          "Insert Log History
*          clear ls_ztvim002.
**          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
*          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
*          if ls_ztvim002-transid is initial.
*            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
*          endif.
*          ls_ztvim002-transtype = 'DPM4'.
*          ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
*          ls_ztvim002-textfile  = ls_dir-name.
*          ls_ztvim002-status    = '2'.
*          ls_ztvim002-notes     = 'Document was already reversed or Document Not Found'.
*          ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
*          ls_ztvim002-erdat     = sy-datum.
*          ls_ztvim002-itime     = sy-uzeit.
*          ls_ztvim002-uname     = sy-uname.
*          modify ztvim002 from ls_ztvim002.
*        else.
*          call function 'BAPI_TRANSACTION_COMMIT'
*            exporting
*              wait = 'X'.
*
*          clear livnpo_ls_ret.
*          read table livnpo_lt_ret into livnpo_ls_ret with key type = 'S'.
*          "Insert Log History
*          clear ls_ztvim002.
*          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
*          ls_ztvim002-transtype = 'DPM4'.
*          ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
*          ls_ztvim002-textfile  = ls_dir-name.
*          ls_ztvim002-status    = '1'.
*          ls_ztvim002-notes     = 'Down Payment Reversed'.
*          ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
*          ls_ztvim002-erdat     = sy-datum.
*          ls_ztvim002-itime     = sy-uzeit.
*          ls_ztvim002-uname     = sy-uname.
*          modify ztvim002 from ls_ztvim002.
*
*          delete dataset file_source.
*        endif.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.
    endif.
  endmethod.                    "revs_dp_npo

  method park_dp_npo.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'DPN1'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'PARK_DP_NPO'.
        refresh : lt_dp_npo01.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_dp_npo01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab
                      into ls_dp_npo01-dpvid
                           ls_dp_npo01-lifnr
                           ls_dp_npo01-bukrs
                           ls_dp_npo01-bldat
                           ls_dp_npo01-budat
                           ls_dp_npo01-waers
                           ls_dp_npo01-xblnr
                           ls_dp_npo01-bktxt
                           ls_dp_npo01-bupla
                           ls_dp_npo01-wrbtr
                           ls_dp_npo01-mwskz
                           ls_dp_npo01-zfbdt
                           ls_dp_npo01-zuonr
                           ls_dp_npo01-sgtxt
                           ls_dp_npo01-witht
                           ls_dp_npo01-wt_withcd
                           ls_dp_npo01-wt_amount.

                    append ls_dp_npo01 to lt_dp_npo01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_dp_npo01 is not initial.

        clear : ls_dp_npo01, ls_dp_npo02.
        read table lt_dp_npo01 into ls_dp_npo01 index  1.

        move-corresponding ls_dp_npo01 to ls_dp_npo02.
        ls_dp_npo02-accot = ls_dp_npo01-lifnr.
        ls_dp_npo02-blart = 'KG'.
        ls_dp_npo02-monat = ls_dp_npo01-budat+4(2).
        ls_dp_npo02-wwert = ls_dp_npo01-bldat.
        ls_dp_npo02-gsber = ls_dp_npo01-bupla.
        ls_dp_npo02-wrbtr = ls_dp_npo02-wrbtr / 100.
        ls_dp_npo02-koart = 'K'.
        ls_dp_npo02-bschl = '29'.
        ls_dp_npo02-xmwst = 'X'.

        if ls_dp_npo01-wt_amount > 0.
          ls_dp_npo01-wt_amount = ls_dp_npo01-wt_amount / 100.
        endif.

        ls_dp_npo02-wt_qssh   = ls_dp_npo01-wt_amount.
        ls_dp_npo02-wt_qsshb  = ls_dp_npo01-wt_amount.
        ls_dp_npo02-wt_basman = 'X'.
        ls_dp_npo02-wt_stat   = 'V'.
        ls_dp_npo02-wt_acco   = ls_dp_npo01-lifnr.

        clear : e_bukrs, e_bukrs, e_gjahr.
        call function 'ZVIM_DP_NPO'
          exporting
            i_data  = ls_dp_npo02
          importing
            e_belnr = e_belnr
            e_bukrs = e_bukrs
            e_gjahr = e_gjahr.

        if e_belnr is not initial and e_bukrs is not initial and e_gjahr is not initial.
          clear xawkey.
*          select single awkey from bkpf into xawkey
*            where bukrs = e_bukrs and belnr = e_belnr and gjahr = e_gjahr.
*
*          if xawkey is initial.
*            concatenate e_belnr e_bukrs into xawkey.
*            concatenate xawkey ' ' e_gjahr into xawkey separated by space.
*          endif.

          concatenate e_belnr e_bukrs into xawkey.
          concatenate xawkey ' ' e_gjahr into xawkey separated by space.

          "Insert Log History
          clear ls_ztvim002.
          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          ls_ztvim002-transtype = 'DPN1'.
          ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = 'Down Payment Created'.
          ls_ztvim002-refrence  = xawkey.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

          clear ls_interface01x.
          ls_interface01x-invid  = ls_dp_npo01-dpvid.
          ls_interface01x-sapdoc = xawkey.
          append ls_interface01x to livpo_returntovim-vim_iv[].

          submit zvim004
            with _invid  = ls_dp_npo01-dpvid
            with _sapdoc = xawkey
            with _bukrs  = e_bukrs
            with _belnr  = e_belnr
            with _gjahr  = e_gjahr
            with _opt2   = ' '
            with _opt4   = ' '
            with _opt3   = 'X'
            with rd_ap1a = 'X' and return.

          delete dataset file_source.
        else.
          clear ls_return.
          read table lt_return into ls_return with key msgty = 'E'.

          clear ls_ztvim002.
          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'DPN1'.
          ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
          concatenate ls_return-msgv1 ls_return-msgv2 ls_return-msgv3 ls_return-msgv4 into ls_ztvim002-notes separated by space.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.
        endif.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.

      if livpo_returntovim is not initial.
        create object o_json.
        o_json->lowercase_names      = abap_true.
        o_json->include_empty_values = abap_true.
        o_json->pretty_print         = abap_true.

        concatenate api_url '/interface/dpnpo' into api_url.
        clear lv_response.
        api_data = o_json->encode( livpo_returntovim ).
        call function 'Z_VIM_CALL_API'
          exporting
            api_data    = api_data
            api_url     = api_url
            api_method  = 'POST'
          importing
            lv_response = lv_response.
      endif.
    endif.
  endmethod.                    "park_dp_npo

  method post_dp_npo.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'DPN2'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'POST_DP_NPO'.
        refresh : lt_dp_npo01.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_dp_npo01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab
                      into ls_dp_npo01-dpvid
                           ls_dp_npo01-lifnr
                           ls_dp_npo01-bukrs
                           ls_dp_npo01-bldat
                           ls_dp_npo01-budat
                           ls_dp_npo01-waers
                           ls_dp_npo01-xblnr
                           ls_dp_npo01-bktxt
                           ls_dp_npo01-bupla
                           ls_dp_npo01-wrbtr
                           ls_dp_npo01-mwskz
                           ls_dp_npo01-zfbdt
                           ls_dp_npo01-zuonr
                           ls_dp_npo01-sgtxt
                           ls_dp_npo01-witht
                           ls_dp_npo01-wt_withcd
                           ls_dp_npo01-wt_amount
                           ls_dp_npo01-sapdoc
                           .

                    append ls_dp_npo01 to lt_dp_npo01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_dp_npo01 is not initial.

        clear : ls_dp_npo01, ls_dp_npo02.
        read table lt_dp_npo01 into ls_dp_npo01 index  1.

        ls_dp_npo02-sapdoc = ls_dp_npo01-sapdoc.

        refresh lt_bdc_return.
        call function 'ZVIM_POST_DP_NPO'
          exporting
            i_data    = ls_dp_npo02
          tables
            lt_return = lt_bdc_return.


        data xbstat type bkpf-bstat.
        clear xbstat.
        select single bstat from bkpf into xbstat
          where bukrs = ls_dp_npo01-bukrs and belnr = ls_dp_npo01-sapdoc(10) and gjahr = ls_dp_npo01-sapdoc+14(4).

        if xbstat <> 'V'.

          "Insert Log History
          clear ls_ztvim002.
          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          ls_ztvim002-transtype = 'DPN2'.
          ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = 'Down Payment Posted'.
          ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

          delete dataset file_source.
        else.
          clear ls_bdc_return.
          read table lt_bdc_return into ls_bdc_return with key msgtyp = 'E'.

          clear ls_ztvim002.

          data :
                msgno type symsgno.
          clear msgno.
          msgno = ls_bdc_return-msgnr.

          clear ls_ztvim002.
          call function 'FORMAT_MESSAGE'
            exporting
              id        = ls_bdc_return-msgid
              lang      = '-D'
              no        = msgno
              v1        = ls_bdc_return-msgv1
              v2        = ls_bdc_return-msgv2
              v3        = ls_bdc_return-msgv3
              v4        = ls_bdc_return-msgv4
            importing
              msg       = ls_ztvim002-notes
            exceptions
              not_found = 1
              others    = 2.

          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'DPN2'.
          ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
          ls_ztvim002-status    = '2'.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.
        endif.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.
    endif.
  endmethod.                    "post_dp_npo

  method delt_dp_npo.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'DPN3'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'CNCL_DP_NPO'.
        refresh : lt_dp_npo01.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_dp_npo01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_dp_npo01-dpvid
                                                ls_dp_npo01-lifnr
                                                ls_dp_npo01-sapdoc
                                                ls_dp_npo01-budat.

                    append ls_dp_npo01 to lt_dp_npo01.
                  endif.
                enddo.
              endif.

            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_dp_npo01 is not initial.

        refresh lt_bdcmsg.
        clear ls_dp_npo01.
        loop at lt_dp_npo01 into ls_dp_npo01.
          call function 'ZVIM_DEL_DP_NPO'
            exporting
              i_header  = ls_dp_npo01
            tables
              lt_return = lt_bdcmsg.

          wait up to 1 seconds.

          clear ls_bdcmsg.
          read table lt_bdcmsg into ls_bdcmsg with key msgtyp = 'E'.
          if sy-subrc <> 0.
            clear ls_ztvim002.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            ls_ztvim002-transtype = 'DPN3'.
            ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '1'.
            ls_ztvim002-notes     = 'Parked Document Deleted'.
            ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.

            call function 'BAPI_TRANSACTION_COMMIT'
              exporting
                wait = 'X'.

            delete dataset file_source. ""delete file
          else.
            clear ls_bdc_return.
            ls_bdc_return = ls_bdcmsg.

            clear ls_ztvim002.

            data :
                  msgno type symsgno.
            clear msgno.
            msgno = ls_bdc_return-msgnr.

            clear ls_ztvim002.
            call function 'FORMAT_MESSAGE'
              exporting
                id        = ls_bdc_return-msgid
                lang      = '-D'
                no        = msgno
                v1        = ls_bdc_return-msgv1
                v2        = ls_bdc_return-msgv2
                v3        = ls_bdc_return-msgv3
                v4        = ls_bdc_return-msgv4
              importing
                msg       = ls_ztvim002-notes
              exceptions
                not_found = 1
                others    = 2.
            select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
            if ls_ztvim002-transid is initial.
              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            endif.
            ls_ztvim002-transtype = 'DPN3'.
            ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '2'.
            ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.
          endif.
          clear ls_liv_npo1.
        endloop.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.
    endif.
  endmethod.                    "delt_dp_npo

  method revs_dp_npo.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'DPN4'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'REVS_DP_NPO'.
        refresh : lt_dp_npo01.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_dp_npo01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_dp_npo01-dpvid
                                                ls_dp_npo01-lifnr
                                                ls_dp_npo01-sapdoc
                                                ls_dp_npo01-bukrs "Reason Reversal
                                                ls_dp_npo01-budat.

                    append ls_dp_npo01 to lt_dp_npo01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_dp_npo01 is not initial.

        data :
              i_bukrs  type rbkp_v-bukrs,
              i_belnr  type rbkp_v-belnr,
              i_gjahr  type rbkp_v-gjahr,
              i_stgrd  type bkpf-stgrd,
              i_budat  type budat.

        data wa_reversal type bapiacrev.
        data wa_bkpf     type bkpf.
        data :
             obj_type	type bapiacrev-obj_type,
             obj_key  type bapiacrev-obj_key,
             obj_sys  type bapiacrev-obj_sys.

        clear ls_dp_npo01.
        read table lt_dp_npo01 into ls_dp_npo01 index 1.

        clear : i_bukrs, i_belnr, i_gjahr, i_stgrd, i_budat.

        i_bukrs = ls_dp_npo01-sapdoc+10(4).
        i_belnr = ls_dp_npo01-sapdoc(10).
        i_gjahr = ls_dp_npo01-sapdoc+14(4).
        i_stgrd = ls_dp_npo01-bukrs.
        i_budat = ls_dp_npo01-budat.

        refresh lt_bdc_return.
        call function 'Z_VIM_REVS_DOC'
          exporting
            i_belnr   = i_belnr
            i_bukrs   = i_bukrs
            i_gjahr   = i_gjahr
            i_stgrd   = i_stgrd
            i_budat   = i_budat
          tables
            lt_return = lt_bdc_return.

        clear ls_bdc_return.
        read table lt_bdc_return into ls_bdc_return with key msgtyp = 'E'.
        if sy-subrc = 0.
          clear ls_ztvim002.

          data :
                msgno type symsgno.
          clear msgno.
          msgno = ls_bdc_return-msgnr.

          clear ls_ztvim002.
          call function 'FORMAT_MESSAGE'
            exporting
              id        = ls_bdc_return-msgid
              lang      = '-D'
              no        = msgno
              v1        = ls_bdc_return-msgv1
              v2        = ls_bdc_return-msgv2
              v3        = ls_bdc_return-msgv3
              v4        = ls_bdc_return-msgv4
            importing
              msg       = ls_ztvim002-notes
            exceptions
              not_found = 1
              others    = 2.

          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'DPN4'.
          ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
          ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

        else.
          clear ls_bkpf.
          select single * from bkpf into ls_bkpf where bukrs = i_bukrs and belnr = i_belnr and gjahr = i_gjahr.
          if ls_bkpf-xreversal = '1' or ls_bkpf-stblg is not initial.
            clear livnpo_ls_ret.
            read table livnpo_lt_ret into livnpo_ls_ret with key type = 'S'.
            "Insert Log History
            clear ls_ztvim002.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            ls_ztvim002-transtype = 'DPN4'.
            ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '1'.
            ls_ztvim002-notes     = 'Down Payment Reversed'.
            ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.

            delete dataset file_source.
          else.
            clear ls_ztvim002.
            select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
            if ls_ztvim002-transid is initial.
              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
            endif.
            ls_ztvim002-transtype = 'DPN4'.
            ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
            ls_ztvim002-textfile  = ls_dir-name.
            ls_ztvim002-status    = '2'.
            ls_ztvim002-notes     = 'Check reversal data'.
            ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
            ls_ztvim002-erdat     = sy-datum.
            ls_ztvim002-itime     = sy-uzeit.
            ls_ztvim002-uname     = sy-uname.
            modify ztvim002 from ls_ztvim002.
          endif.
        endif.

*        call function 'CALL_FB08'
*          exporting
*            i_bukrs      = i_bukrs
*            i_belnr      = i_belnr
*            i_gjahr      = i_gjahr
*            i_stgrd      = i_stgrd
*            i_budat      = i_budat
**           I_MONAT      =
*          exceptions
*            not_possible = 1
*            others       = 2.
*
*        if sy-subrc <> 0.
*          "Insert Log History
*          clear ls_ztvim002.
*          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
*          if ls_ztvim002-transid is initial.
*            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
*          endif.
*          ls_ztvim002-transtype = 'DPN4'.
*          ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
*          ls_ztvim002-textfile  = ls_dir-name.
*          ls_ztvim002-status    = '2'.
*          ls_ztvim002-notes     = 'Document was already reversed or Document Not Found'.
*          ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
*          ls_ztvim002-erdat     = sy-datum.
*          ls_ztvim002-itime     = sy-uzeit.
*          ls_ztvim002-uname     = sy-uname.
*          modify ztvim002 from ls_ztvim002.
*        else.
*
*          call function 'BAPI_TRANSACTION_COMMIT'
*            exporting
*              wait = 'X'.
*
*          data ls_bkpf type bkpf.
*          clear ls_bkpf.
*          select single * from bkpf into ls_bkpf where bukrs = i_bukrs and belnr = i_belnr and gjahr = i_gjahr.
*          if ls_bkpf-xreversal = '1' or ls_bkpf-stblg is not initial.
*            clear livnpo_ls_ret.
*            read table livnpo_lt_ret into livnpo_ls_ret with key type = 'S'.
*            "Insert Log History
*            clear ls_ztvim002.
*            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
*            ls_ztvim002-transtype = 'DPN4'.
*            ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
*            ls_ztvim002-textfile  = ls_dir-name.
*            ls_ztvim002-status    = '1'.
*            ls_ztvim002-notes     = 'Down Payment Reversed'.
*            ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
*            ls_ztvim002-erdat     = sy-datum.
*            ls_ztvim002-itime     = sy-uzeit.
*            ls_ztvim002-uname     = sy-uname.
*            modify ztvim002 from ls_ztvim002.
*
*            delete dataset file_source.
*          else.
*            clear ls_ztvim002.
*            select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
*            if ls_ztvim002-transid is initial.
*              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
*            endif.
*            ls_ztvim002-transtype = 'DPN4'.
*            ls_ztvim002-lifnr     = ls_dp_npo01-lifnr.
*            ls_ztvim002-textfile  = ls_dir-name.
*            ls_ztvim002-status    = '2'.
*            ls_ztvim002-notes     = 'Check reversal data'.
*            ls_ztvim002-refrence  = ls_dp_npo01-sapdoc.
*            ls_ztvim002-erdat     = sy-datum.
*            ls_ztvim002-itime     = sy-uzeit.
*            ls_ztvim002-uname     = sy-uname.
*            modify ztvim002 from ls_ztvim002.
*          endif.
*        endif.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.
    endif.
  endmethod.                    "revs_dp_npo

  method park_credit_note.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'CRN1'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear   : ls_dir, livpo_lt_rbco.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[], livpo_returntovim-vim_iv[],
                livpo_returntovim-bkpf[], livpo_returntovim-bseg[], livpo_returntovim-rbco[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'PARK_PO' and name ns 'CLAIM'.
        refresh : lt_interface01, lt_interface01x.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_interface01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_interface01-lifnr
                                                ls_interface01-invid
                                                ls_interface01-bukrs
                                                ls_interface01-bldat
                                                ls_interface01-budat
                                                ls_interface01-bupla
                                                ls_interface01-xblnr
                                                ls_interface01-sgtxt
                                                ls_interface01-mwskz1
                                                ls_interface01-rmwwr
                                                ls_interface01-waers
                                                ls_interface01-zfbdt
                                                ls_interface01-zterm
                                                ls_interface01-zlspr
                                                ls_interface01-zuonr
                                                ls_interface01-bktxt
                                                ls_interface01-ebeln
                                                ls_interface01-ebelp
                                                ls_interface01-mblnr
                                                ls_interface01-mjahr
                                                ls_interface01-zeile
                                                ls_interface01-dmbtr
                                                ls_interface01-menge
                                                ls_interface01-meins
                                                ls_interface01-witht
                                                ls_interface01-wt_withcd
                                                ls_interface01-wt_amount
                                                ls_interface01-hkont
                                                ls_interface01-adjamount
                                                ls_interface01-adtxcode
                                                ls_interface01-adtext
                                                ls_interface01-kostl
                                                .

                    ls_interface01-gsber = ls_interface01-bupla.

                    call function 'CONVERSION_EXIT_ALPHA_INPUT'
                      exporting
                        input  = ls_interface01-ebelp
                      importing
                        output = ls_interface01-ebelp.

                    call function 'CONVERSION_EXIT_ALPHA_INPUT'
                      exporting
                        input  = ls_interface01-zeile
                      importing
                        output = ls_interface01-zeile.

                    append ls_interface01 to lt_interface01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_interface01 is not initial.

        refresh : lt_interface01x, lt_interface01gl, lt_interface01wt, lt_ekbz, lt_interface01z, lt_lfbw.
        append lines of lt_interface01 to lt_interface01x.
        append lines of lt_interface01 to lt_interface01gl.
        append lines of lt_interface01 to lt_interface01wt.
        append lines of lt_interface01 to lt_interface01z.

        sort lt_interface01x  by lifnr invid.
        sort lt_interface01gl by lifnr invid hkont adjamount adtxcode kostl.
        sort lt_interface01wt by lifnr invid witht wt_withcd wt_amount.
        sort lt_interface01   by lifnr invid ebeln ebelp mblnr mjahr zeile.
        sort lt_interface01z  by lifnr invid ebeln ebelp.
        delete adjacent duplicates from lt_interface01x  comparing lifnr invid.
        delete adjacent duplicates from lt_interface01gl comparing lifnr invid hkont adjamount adtxcode kostl.
        delete adjacent duplicates from lt_interface01wt comparing lifnr invid witht wt_withcd wt_amount.
        delete adjacent duplicates from lt_interface01   comparing lifnr invid ebeln ebelp mblnr mjahr zeile.
        delete adjacent duplicates from lt_interface01z  comparing lifnr invid ebeln ebelp.


        select * from ekbz into table lt_ekbz
          for all entries in lt_interface01[]
            where ebeln = lt_interface01-ebeln and ebelp = lt_interface01-ebelp and
            belnr = lt_interface01-mblnr and gjahr = lt_interface01-mjahr and buzei = lt_interface01-zeile
            and vgabe = '1' and shkzg = 'H'.

        select * from lfbw into table lt_lfbw
          for all entries in lt_interface01[]
            where lifnr = lt_interface01-lifnr and bukrs = lt_interface01-bukrs.

        data ls_mblnr.
        clear ls_ekbz.
        loop at lt_ekbz into ls_ekbz.
          clear ls_mblnr.
          select single mblnr from mseg into ls_mblnr
            where smbln = ls_ekbz-belnr and sjahr = ls_ekbz-gjahr and smblp = ls_ekbz-buzei and bwart = '102'.
          if ls_mblnr is not initial.
            delete lt_ekbz where belnr = ls_ekbz-belnr and gjahr = ls_ekbz-gjahr and buzei = ls_ekbz-buzei.
          endif.
          clear ls_ekbz.
        endloop.

        refresh lt_ekbzx.
        clear ls_ekbz.
        loop at lt_ekbz into ls_ekbz.
          clear ls_ekbzx.
          read table lt_ekbzx into ls_ekbzx with key ebeln = ls_ekbz-ebeln ebelp = ls_ekbz-ebelp.
          if sy-subrc = 0.
            ls_ekbzx-wrbtr = ls_ekbzx-wrbtr + ls_ekbz-wrbtr.
            ls_ekbzx-dmbtr = ls_ekbzx-dmbtr + ls_ekbz-dmbtr.
            ls_ekbzx-menge = ls_ekbzx-menge + ls_ekbz-menge.

            modify lt_ekbzx from ls_ekbzx transporting dmbtr wrbtr menge where ebeln = ls_ekbz-ebeln and ebelp = ls_ekbz-ebelp.
          else.
            append ls_ekbz to lt_ekbzx.
          endif.
          clear ls_ekbz.
        endloop.

        refresh livpo_lt_taxdata.
        clear ls_interface01x.
        loop at lt_interface01x into ls_interface01x.

          clear livpo_headerdata.
          livpo_headerdata-invoice_ind    = ' '.
          livpo_headerdata-doc_type       = 'RE'.
          livpo_headerdata-doc_date       = ls_interface01x-bldat.
          livpo_headerdata-pstng_date     = ls_interface01x-budat.
          livpo_headerdata-ref_doc_no     = ls_interface01x-xblnr.
          livpo_headerdata-comp_code      = ls_interface01x-bukrs.
          livpo_headerdata-currency       = ls_interface01x-waers.
          livpo_headerdata-bline_date     = ls_interface01x-zfbdt.
          livpo_headerdata-pmnttrms       = ls_interface01x-zterm.
          livpo_headerdata-pmnt_block     = ls_interface01x-zlspr.
          livpo_headerdata-header_txt     = ls_interface01x-bktxt.
          livpo_headerdata-alloc_nmbr     = ls_interface01x-zuonr.
          livpo_headerdata-bus_area       = ls_interface01x-gsber.
          livpo_headerdata-calc_tax_ind   = 'X'.
          livpo_headerdata-del_costs_taxc = ls_interface01x-mwskz1.
          livpo_headerdata-business_place = ls_interface01x-bupla.
          livpo_headerdata-item_text      = ls_interface01x-sgtxt.

          clear ls_interface01.
          refresh : livpo_lt_itemdata, lt_cstekpo, livpo_lt_withtaxdata, livpo_lt_glaccountdata.
          data docitem type i.
          docitem = 0.

          clear : customstr, xtotalpo.
          loop at lt_interface01 into ls_interface01
            where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid.
            clear livpo_ls_itemdata.
            docitem = docitem + 1.
            livpo_ls_itemdata-invoice_doc_item = docitem.
            livpo_ls_itemdata-po_number        = ls_interface01-ebeln.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = ls_interface01-ebelp
              importing
                output = livpo_ls_itemdata-po_item.

            livpo_ls_itemdata-ref_doc          = ls_interface01-mblnr.
            livpo_ls_itemdata-ref_doc_year     = ls_interface01-mjahr.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = ls_interface01-zeile
              importing
                output = livpo_ls_itemdata-ref_doc_it.


            livpo_ls_itemdata-tax_code         = ls_interface01-mwskz1.
            livpo_ls_itemdata-quantity         = ls_interface01-menge.
            livpo_ls_itemdata-po_unit          = ls_interface01-meins.

            clear : ls_t685t, ls_ekbz.
            read table lt_ekbz into ls_ekbz
              with key ebeln = ls_interface01-ebeln ebelp = ls_interface01-ebelp
              belnr = ls_interface01-mblnr gjahr = ls_interface01-mjahr buzei = ls_interface01-zeile.

            if ls_ekbz-waers = 'IDR'.
              ls_ekbz-dmbtr = ls_ekbz-dmbtr * 100.
            endif.


            if ls_ekbz is not initial.
              ls_interface01-dmbtr      = ls_interface01-dmbtr - ls_ekbz-dmbtr.
            endif.
            condense ls_interface01-dmbtr.
            livpo_ls_itemdata-item_amount      = ls_interface01-dmbtr.

            xtotalpo = xtotalpo + ls_interface01-dmbtr.

            append livpo_ls_itemdata to livpo_lt_itemdata.

            clear ls_ekpo.
            ls_ekpo-ebeln = ls_interface01-ebeln.
            append ls_ekpo to lt_cstekpo.
            clear ls_interface01.
          endloop.

          "Append PO Condition
          clear : ls_interface01, xtotalppkb.
          loop at lt_interface01z into ls_interface01
            where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid..
            clear : ls_t685t, ls_ekbz.
            read table lt_ekbzx into ls_ekbz
              with key ebeln = ls_interface01-ebeln ebelp = ls_interface01-ebelp.

            if ls_ekbz-waers = 'IDR'.
              ls_ekbz-dmbtr = ls_ekbz-dmbtr * 100.
            endif.

            if ls_ekbz is not initial.
              clear livpo_ls_itemdata.
              docitem = docitem + 1.
              livpo_ls_itemdata-invoice_doc_item = docitem.
              livpo_ls_itemdata-po_number        = ls_interface01-ebeln.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = ls_interface01-ebelp
                importing
                  output = livpo_ls_itemdata-po_item.

              livpo_ls_itemdata-ref_doc          = ls_interface01-mblnr.
              livpo_ls_itemdata-ref_doc_year     = ls_interface01-mjahr.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = ls_interface01-zeile
                importing
                  output = livpo_ls_itemdata-ref_doc_it.

              livpo_ls_itemdata-tax_code         = 'N0'.
              livpo_ls_itemdata-quantity         = ls_ekbz-menge.
              livpo_ls_itemdata-po_unit          = ls_interface01-meins.
              livpo_ls_itemdata-item_amount      = ls_ekbz-dmbtr.
              livpo_ls_itemdata-cond_type        = ls_ekbz-kschl.

              xtotalppkb = xtotalppkb + ls_ekbz-dmbtr.

              append livpo_ls_itemdata to livpo_lt_itemdata.
            endif.

            clear ls_interface01.
          endloop.

          "Append Adjustment Data
          clear :ls_interface01gl, docitem, xtotaladj.
          loop at lt_interface01gl into ls_interface01gl where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid.
            clear livpo_ls_glaccountdata.
            if ls_interface01gl-hkont > 0 and ls_interface01gl-kostl is not initial.
              docitem = docitem + 1.
              livpo_ls_glaccountdata-invoice_doc_item = docitem.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = ls_interface01gl-hkont
                importing
                  output = ls_interface01gl-hkont.
              livpo_ls_glaccountdata-gl_account       = ls_interface01gl-hkont.
              livpo_ls_glaccountdata-db_cr_ind        = 'H'.
              livpo_ls_glaccountdata-item_amount      = ls_interface01gl-adjamount.
              livpo_ls_glaccountdata-comp_code        = ls_interface01gl-bukrs.
              livpo_ls_glaccountdata-alloc_nmbr       = ls_interface01gl-zuonr.
              livpo_ls_glaccountdata-item_text        = ls_interface01gl-adtext.
              livpo_ls_glaccountdata-tax_code         = ls_interface01gl-adtxcode.
*              call function 'CONVERSION_EXIT_ALPHA_INPUT'
*                exporting
*                  input  = ls_interface01gl-kostl
*                importing
*                  output = livpo_ls_glaccountdata-costcenter.
              if ls_interface01gl-adtxcode = 'V1' or ls_interface01gl-adtxcode = 'VA'.
                xtotaladj = xtotaladj + ( livpo_ls_glaccountdata-item_amount + ( livpo_ls_glaccountdata-item_amount * ( 10 / 100 ) ) ).
              elseif ls_interface01gl-adtxcode = 'V2'.
                xtotaladj = xtotaladj + ( livpo_ls_glaccountdata-item_amount + ( livpo_ls_glaccountdata-item_amount * ( 1 / 100 ) ) ).
              else.
                xtotaladj = xtotaladj + livpo_ls_glaccountdata-item_amount.
              endif.
              livpo_ls_glaccountdata-costcenter       = ls_interface01gl-kostl.


              append livpo_ls_glaccountdata to livpo_lt_glaccountdata.
            endif.
            clear ls_interface01gl.
          endloop.

          "Append Witholdingtax Data
          clear : ls_interface01wt, docitem.
          loop at lt_interface01wt into ls_interface01wt where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid.
            clear livpo_ls_withtaxdata.
            if ls_interface01wt-witht is not initial.
              docitem = docitem + 1.
              livpo_ls_withtaxdata-split_key   = docitem.
              livpo_ls_withtaxdata-wi_tax_type = ls_interface01wt-witht.
              livpo_ls_withtaxdata-wi_tax_code = ls_interface01wt-wt_withcd.
              livpo_ls_withtaxdata-wi_tax_base = ls_interface01wt-wt_amount.
              append livpo_ls_withtaxdata to livpo_lt_withtaxdata.
            endif.
            clear ls_interface01wt.
          endloop.

          if livpo_lt_withtaxdata is initial.
            clear ls_lfbw.
            loop at lt_lfbw into ls_lfbw where lifnr = ls_interface01x-lifnr and bukrs = ls_interface01x-bukrs.
              clear livpo_ls_withtaxdata.
              docitem = docitem + 1.
              livpo_ls_withtaxdata-split_key   = docitem.
              livpo_ls_withtaxdata-wi_tax_type = ls_lfbw-witht.
              livpo_ls_withtaxdata-wi_tax_code = ''.
              append livpo_ls_withtaxdata to livpo_lt_withtaxdata.
              clear ls_lfbw.
            endloop.
          endif.

          clear ls_interface01x.
        endloop.

        clear ls_interface01x.
        read table lt_interface01x into ls_interface01x index 1.

        if ls_interface01x-mwskz1 = 'V1' or ls_interface01x-mwskz1 = 'VA'.
          livpo_headerdata-gross_amount = xtotalpo + xtotaladj + ( xtotalpo * ( 10 / 100 ) ).
        elseif ls_interface01x-mwskz1 = 'V2'.
          livpo_headerdata-gross_amount = xtotalpo + xtotaladj + ( xtotalpo * ( 1 / 100 ) ).
        else.
          livpo_headerdata-gross_amount = xtotalpo + xtotaladj.
        endif.

        livpo_headerdata-gross_amount = livpo_headerdata-gross_amount + xtotalppkb.

        if ls_interface01x-waers = 'IDR'.
          livpo_headerdata-gross_amount = round( val = livpo_headerdata-gross_amount dec = 0 mode = 5 ).
        endif.

        if livpo_headerdata is initial.
          exit.
        endif.

        call function 'BAPI_INCOMINGINVOICE_PARK'
          exporting
            headerdata       = livpo_headerdata
          importing
            invoicedocnumber = livpo_invoicedocnumber
            fiscalyear       = livpo_fiscalyear
          tables
            itemdata         = livpo_lt_itemdata
            accountingdata   = livpo_lt_accountingdata
            glaccountdata    = livpo_lt_glaccountdata
            taxdata          = livpo_lt_taxdata
            withtaxdata      = livpo_lt_withtaxdata
            return           = livpo_lt_return.

        if livpo_invoicedocnumber is not initial.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.


          clear xawkey.
          refresh : livpo_lt_rbkp, livpo_lt_rseg, livpo_lt_bkpf, livpo_lt_bseg, livpo_lt_rbco.

          concatenate livpo_invoicedocnumber livpo_fiscalyear into xawkey.

          clear ls_interface01x.
          read table lt_interface01x into ls_interface01x index 1.
          concatenate livpo_invoicedocnumber livpo_fiscalyear into ls_interface01x-sapdoc.
          append ls_interface01x to livpo_returntovim-vim_iv[].

*          select * from rbkp into corresponding fields of table livpo_lt_rbkp
*            where belnr = livpo_invoicedocnumber and gjahr = livpo_fiscalyear
*                  and bukrs = ls_interface01x-bukrs and lifnr = ls_interface01x-lifnr.
*
*          select * from rseg into corresponding fields of table livpo_lt_rseg
*            where belnr = livpo_invoicedocnumber and gjahr = livpo_fiscalyear
*                  and bukrs = ls_interface01x-bukrs.
*
*          select * from rbco into corresponding fields of table livpo_lt_rbco
*            where belnr = livpo_invoicedocnumber and gjahr = livpo_fiscalyear
*                  and bukrs = ls_interface01x-bukrs.

          select * from bkpf into corresponding fields of table livpo_lt_bkpf
            where awkey = xawkey and bukrs = ls_interface01x-bukrs.

          if livpo_lt_bkpf is not initial.
            select * from bseg into corresponding fields of table livpo_lt_bseg
              for all entries in livpo_lt_bkpf
                where bukrs = livpo_lt_bkpf-bukrs and belnr = livpo_lt_bkpf-belnr and gjahr = livpo_lt_bkpf-gjahr.
          endif.

*          append lines of livpo_lt_rbkp to livpo_returntovim-rbkp[].
*          append lines of livpo_lt_rseg to livpo_returntovim-rseg[].
*          append lines of livpo_lt_bkpf to livpo_returntovim-bkpf[].
*          append lines of livpo_lt_bseg to livpo_returntovim-bseg[].
*          append lines of livpo_lt_rbco to livpo_returntovim-rbco[].

          refresh t_drseg.
          clear   e_rbkpv.
          call function 'MRM_INVOICE_READ'
            exporting
              i_belnr         = livpo_invoicedocnumber
              i_gjahr         = livpo_fiscalyear
            importing
              e_rbkpv         = e_rbkpv
            tables
              t_drseg         = t_drseg
            exceptions
              entry_not_found = 1
              lock_error      = 2
              others          = 3.

          loop at t_drseg assigning <fs_drseg>.
            <fs_drseg>-ok = 'X'.
          endloop.

          data :
                i_rbkpv  type  mrm_rbkpv,
                ls_xupda type  marke,
                ls_rbstat_new  type  rbstat,
                ti_drseg  type  mmcr_tdrseg.

          ls_xupda      = 'U'.
          ls_rbstat_new = 'A'.

          call function 'MRM_INVOICE_PARK'
            exporting
              i_rbkpv           = e_rbkpv
              i_xupda           = ls_xupda
              i_rbstat_new      = ls_rbstat_new
              ti_drseg          = t_drseg
            exceptions
              invalid_status    = 1
              update_impossible = 2
              user_exit         = 3
              error_message     = 99
              others            = 4.

          "Insert Log History
          clear ls_ztvim002.
          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          ls_ztvim002-transtype = 'CRN1'.
          ls_ztvim002-lifnr     = ls_interface01x-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = 'Credit Memo Parked'.
          concatenate livpo_invoicedocnumber livpo_fiscalyear into ls_ztvim002-refrence.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.

          clear : ls_interface01x, livpo_ls_bkpf.
          read table lt_interface01x into ls_interface01x index 1.

          read table livpo_lt_bkpf into livpo_ls_bkpf index 1.
          concatenate livpo_invoicedocnumber livpo_fiscalyear into ls_interface01x-sapdoc.
          submit zvim004
            with _invid  = ls_interface01x-invid
            with _sapdoc = ls_interface01x-sapdoc
            with _bukrs  = ls_interface01x-bukrs
            with _belnr  = livpo_ls_bkpf-belnr
            with _gjahr  = livpo_ls_bkpf-gjahr
            with _opt2   = ' '
            with _opt4   = ' '
            with _opt3   = 'X'
            with rd_ap1a = 'X' and return.

          delete dataset file_source.
        else.
          clear livpo_ls_return.
          read table livpo_lt_return into livpo_ls_return index 1.
          "Insert Log History
          clear ls_ztvim002.
*          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'CRN1'.
          ls_ztvim002-lifnr     = ls_interface01x-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
          ls_ztvim002-notes     = livpo_ls_return-message.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.
        endif.

        close dataset file_source. "close file
        close dataset dst_source.  "close file
        clear ls_dir.
      endloop.

      create object o_json.
      o_json->lowercase_names      = abap_true.
      o_json->include_empty_values = abap_true.
      o_json->pretty_print         = abap_true.

      concatenate api_url '/interface/creditmemo' into api_url.

      api_data = o_json->encode( livpo_returntovim ).
      call function 'Z_VIM_CALL_API'
        exporting
          api_data    = api_data
          api_url     = api_url
          api_method  = 'POST'
        importing
          lv_response = lv_response.
    endif.
  endmethod.                    "park_credit_note

  method post_credit_note.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'CRN2'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear : ls_dir, livpo_returntovim.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[], livpo_returntovim-vim_iv[],
                livpo_returntovim-bkpf[], livpo_returntovim-bseg[], livpo_returntovim-rbco[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'POST_CN' and name ns 'CLAIM'.
        refresh : lt_interface01, lt_interface01x.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_interface01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_interface01-lifnr
                                                ls_interface01-invid
                                                ls_interface01-bukrs
                                                ls_interface01-bldat
                                                ls_interface01-budat
                                                ls_interface01-bupla
                                                ls_interface01-xblnr
                                                ls_interface01-sgtxt
                                                ls_interface01-mwskz1
                                                ls_interface01-rmwwr
                                                ls_interface01-waers
                                                ls_interface01-zfbdt
                                                ls_interface01-zterm
                                                ls_interface01-zlspr
                                                ls_interface01-zuonr
                                                ls_interface01-bktxt
                                                ls_interface01-sapdoc

                                                ls_interface01-ebeln
                                                ls_interface01-ebelp
                                                ls_interface01-mblnr
                                                ls_interface01-mjahr
                                                ls_interface01-zeile
                                                ls_interface01-dmbtr
                                                ls_interface01-menge
                                                ls_interface01-meins

                                                ls_interface01-witht
                                                ls_interface01-wt_withcd
                                                ls_interface01-wt_amount
                                                ls_interface01-hkont
                                                ls_interface01-adjamount
                                                ls_interface01-adtxcode
                                                ls_interface01-adtext
                                                ls_interface01-kostl.

                    ls_interface01-gsber = ls_interface01-bupla.
                    call function 'CONVERSION_EXIT_ALPHA_INPUT'
                      exporting
                        input  = ls_interface01-ebelp
                      importing
                        output = ls_interface01-ebelp.

                    call function 'CONVERSION_EXIT_ALPHA_INPUT'
                      exporting
                        input  = ls_interface01-zeile
                      importing
                        output = ls_interface01-zeile.
                    append ls_interface01 to lt_interface01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_interface01 is not initial.

        refresh : lt_interface01x, lt_interface01gl, lt_interface01wt, lt_ekbz, lt_interface01z.
        append lines of lt_interface01 to lt_interface01x.
        append lines of lt_interface01 to lt_interface01gl.
        append lines of lt_interface01 to lt_interface01wt.
        append lines of lt_interface01 to lt_interface01z.

        sort lt_interface01x  by lifnr invid.
        sort lt_interface01gl by lifnr invid hkont adjamount adtxcode kostl.
        sort lt_interface01wt by lifnr invid witht wt_withcd wt_amount.
        sort lt_interface01   by lifnr invid ebeln ebelp mblnr mjahr zeile.
        sort lt_interface01z  by lifnr invid ebeln ebelp.
        delete adjacent duplicates from lt_interface01x  comparing lifnr invid.
        delete adjacent duplicates from lt_interface01gl comparing lifnr invid hkont adjamount adtxcode kostl.
        delete adjacent duplicates from lt_interface01wt comparing lifnr invid witht wt_withcd wt_amount.
        delete adjacent duplicates from lt_interface01   comparing lifnr invid ebeln ebelp mblnr mjahr zeile.
        delete adjacent duplicates from lt_interface01z  comparing lifnr invid ebeln ebelp.


        select * from ekbz into table lt_ekbz
          for all entries in lt_interface01[]
            where ebeln = lt_interface01-ebeln and ebelp = lt_interface01-ebelp and
            belnr = lt_interface01-mblnr and gjahr = lt_interface01-mjahr and buzei = lt_interface01-zeile
            and vgabe = '1' and shkzg = 'H'.

        refresh lt_lfbw.
        select * from lfbw into table lt_lfbw
          for all entries in lt_interface01[]
            where lifnr = lt_interface01-lifnr and bukrs = lt_interface01-bukrs.

        data ls_mblnr.
        clear ls_ekbz.
        loop at lt_ekbz into ls_ekbz.
          clear ls_mblnr.
          select single mblnr from mseg into ls_mblnr
            where smbln = ls_ekbz-belnr and sjahr = ls_ekbz-gjahr and smblp = ls_ekbz-buzei and bwart = '102'.
          if ls_mblnr is not initial.
            delete lt_ekbz where belnr = ls_ekbz-belnr and gjahr = ls_ekbz-gjahr and buzei = ls_ekbz-buzei.
          endif.
          clear ls_ekbz.
        endloop.

        refresh lt_ekbzx.
        clear ls_ekbz.
        loop at lt_ekbz into ls_ekbz.
          clear ls_ekbzx.
          read table lt_ekbzx into ls_ekbzx with key ebeln = ls_ekbz-ebeln ebelp = ls_ekbz-ebelp.
          if sy-subrc = 0.
            ls_ekbzx-wrbtr = ls_ekbzx-wrbtr + ls_ekbz-wrbtr.
            ls_ekbzx-dmbtr = ls_ekbzx-dmbtr + ls_ekbz-dmbtr.
            ls_ekbzx-menge = ls_ekbzx-menge + ls_ekbz-menge.

            modify lt_ekbzx from ls_ekbzx transporting dmbtr wrbtr menge where ebeln = ls_ekbz-ebeln and ebelp = ls_ekbz-ebelp.
          else.
            append ls_ekbz to lt_ekbzx.
          endif.
          clear ls_ekbz.
        endloop.

        refresh livpo_lt_taxdata.

        clear ls_interface01x.
        loop at lt_interface01x into ls_interface01x.
          data :
              i_belnr  type rbkp_v-belnr,
              i_gjahr  type rbkp_v-gjahr,
              i_belnr2  type bkpf-belnr,
              i_gjahr2  type bkpf-gjahr,
              e_output type string,
              e_msgtyp type string.

          clear : i_belnr, i_gjahr, e_output.
          i_belnr = ls_interface01x-sapdoc(10).
          i_gjahr = ls_interface01x-sapdoc+10(4).

          refresh livpo_lt_return.
          call function 'BAPI_INCOMINGINVOICE_GETDETAIL'
            exporting
              invoicedocnumber = i_belnr
              fiscalyear       = i_gjahr
            importing
              headerdata       = headerdata_dtl
            tables
              itemdata         = lt_itemdata_dtl
              accountingdata   = lt_accountingdata_dtl
              glaccountdata    = lt_glaccountdata_dtl
              taxdata          = lt_taxdata_dtl
              withtaxdata      = lt_withtaxdata_dtl
              return           = livpo_lt_return.


          refresh livpo_lt_glaccountdata.
          if sy-subrc = 0.

            clear ls_interface01.
            refresh : livpo_lt_itemdata, lt_cstekpo, livpo_lt_withtaxdata, livpo_lt_glaccountdata.
            data docitem type i.
            docitem = 0.

            clear : customstr, xtotalpo.
            loop at lt_interface01 into ls_interface01
              where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid.
              clear livpo_ls_itemdata.
              docitem = docitem + 1.
              livpo_ls_itemdata-invoice_doc_item = docitem.
              livpo_ls_itemdata-po_number        = ls_interface01-ebeln.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = ls_interface01-ebelp
                importing
                  output = livpo_ls_itemdata-po_item.

              livpo_ls_itemdata-ref_doc          = ls_interface01-mblnr.
              livpo_ls_itemdata-ref_doc_year     = ls_interface01-mjahr.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = ls_interface01-zeile
                importing
                  output = livpo_ls_itemdata-ref_doc_it.

              livpo_ls_itemdata-tax_code         = ls_interface01-mwskz1.
              livpo_ls_itemdata-quantity         = ls_interface01-menge.
              livpo_ls_itemdata-po_unit          = ls_interface01-meins.

              clear : ls_t685t, ls_ekbz.
              read table lt_ekbz into ls_ekbz
                with key ebeln = ls_interface01-ebeln ebelp = ls_interface01-ebelp
                belnr = ls_interface01-mblnr gjahr = ls_interface01-mjahr buzei = ls_interface01-zeile.

              if ls_ekbz-waers = 'IDR'.
                ls_ekbz-dmbtr = ls_ekbz-dmbtr * 100.
              endif.


              if ls_ekbz is not initial.
                ls_interface01-dmbtr      = ls_interface01-dmbtr - ls_ekbz-dmbtr.
              endif.
              xtotalpo = xtotalpo + ls_interface01-dmbtr.
              condense ls_interface01-dmbtr.
              livpo_ls_itemdata-item_amount      = ls_interface01-dmbtr.
              append livpo_ls_itemdata to livpo_lt_itemdata.

              clear ls_ekpo.
              ls_ekpo-ebeln = ls_interface01-ebeln.
              append ls_ekpo to lt_cstekpo.
              clear ls_interface01.
            endloop.

            "Append PO Condition
            clear : ls_interface01, xtotalppkb.
            loop at lt_interface01z into ls_interface01
              where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid..
              clear : ls_t685t, ls_ekbz.
              read table lt_ekbzx into ls_ekbz
                with key ebeln = ls_interface01-ebeln ebelp = ls_interface01-ebelp.

              if ls_ekbz-waers = 'IDR'.
                ls_ekbz-dmbtr = ls_ekbz-dmbtr * 100.
              endif.

              if ls_ekbz is not initial.
                clear livpo_ls_itemdata.
                docitem = docitem + 1.
                livpo_ls_itemdata-invoice_doc_item = docitem.
                livpo_ls_itemdata-po_number        = ls_interface01-ebeln.
                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = ls_interface01-ebelp
                  importing
                    output = livpo_ls_itemdata-po_item.

                livpo_ls_itemdata-ref_doc          = ls_interface01-mblnr.
                livpo_ls_itemdata-ref_doc_year     = ls_interface01-mjahr.
                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = ls_interface01-zeile
                  importing
                    output = livpo_ls_itemdata-ref_doc_it.

                livpo_ls_itemdata-tax_code         = 'N0'.
                livpo_ls_itemdata-quantity         = ls_ekbz-menge.
                livpo_ls_itemdata-po_unit          = ls_interface01-meins.
                livpo_ls_itemdata-item_amount      = ls_ekbz-dmbtr.
                livpo_ls_itemdata-cond_type        = ls_ekbz-kschl.
                xtotalppkb = xtotalppkb + ls_ekbz-dmbtr.

                append livpo_ls_itemdata to livpo_lt_itemdata.
              endif.

              clear ls_interface01.
            endloop.

            "Append Adjustment Data
            refresh livpo_lt_glaccountdata.
            clear :ls_interface01gl, docitem, xtotaladj.
            loop at lt_interface01gl into ls_interface01gl where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid.
              clear livpo_ls_glaccountdata.
              if ls_interface01gl-hkont > 0 and ls_interface01gl-kostl is not initial.
                docitem = docitem + 1.
                livpo_ls_glaccountdata-invoice_doc_item = docitem.
                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = ls_interface01gl-hkont
                  importing
                    output = ls_interface01gl-hkont.
                livpo_ls_glaccountdata-gl_account       = ls_interface01gl-hkont.
                livpo_ls_glaccountdata-db_cr_ind        = 'H'.
                livpo_ls_glaccountdata-item_amount      = ls_interface01gl-adjamount.
                livpo_ls_glaccountdata-comp_code        = ls_interface01gl-bukrs.
                livpo_ls_glaccountdata-alloc_nmbr       = ls_interface01gl-zuonr.
                livpo_ls_glaccountdata-item_text        = ls_interface01gl-adtext.
                livpo_ls_glaccountdata-tax_code         = ls_interface01gl-adtxcode.

                if ls_interface01gl-adtxcode = 'V1' or ls_interface01gl-adtxcode = 'VA'.
                  xtotaladj = xtotaladj + ( livpo_ls_glaccountdata-item_amount + ( livpo_ls_glaccountdata-item_amount * ( 10 / 100 ) ) ).
                elseif ls_interface01gl-adtxcode = 'V2'.
                  xtotaladj = xtotaladj + ( livpo_ls_glaccountdata-item_amount + ( livpo_ls_glaccountdata-item_amount * ( 1 / 100 ) ) ).
                else.
                  xtotaladj = xtotaladj + livpo_ls_glaccountdata-item_amount.
                endif.
                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = ls_interface01gl-kostl
                  importing
                    output = livpo_ls_glaccountdata-costcenter.

                append livpo_ls_glaccountdata to livpo_lt_glaccountdata.
              endif.
              clear ls_interface01gl.
            endloop.

            "Append Witholdingtax Data
            refresh livpo_lt_withtaxdata.
            clear : ls_interface01wt, docitem.
            loop at lt_interface01wt into ls_interface01wt where lifnr = ls_interface01x-lifnr and invid = ls_interface01x-invid.
              clear livpo_ls_withtaxdata.
              if ls_interface01wt-witht is not initial.
                docitem = docitem + 1.
                livpo_ls_withtaxdata-split_key   = docitem.
                livpo_ls_withtaxdata-wi_tax_type = ls_interface01wt-witht.
                livpo_ls_withtaxdata-wi_tax_code = ls_interface01wt-wt_withcd.
                livpo_ls_withtaxdata-wi_tax_base = ls_interface01wt-wt_amount.
                append livpo_ls_withtaxdata to livpo_lt_withtaxdata.
              endif.
              clear ls_interface01wt.
            endloop.

            if livpo_lt_withtaxdata is initial.
              clear ls_lfbw.
              loop at lt_lfbw into ls_lfbw where lifnr = ls_interface01x-lifnr and bukrs = ls_interface01x-bukrs.
                clear livpo_ls_withtaxdata.
                docitem = docitem + 1.
                livpo_ls_withtaxdata-split_key   = docitem.
                livpo_ls_withtaxdata-wi_tax_type = ls_lfbw-witht.
                livpo_ls_withtaxdata-wi_tax_code = ''.
                append livpo_ls_withtaxdata to livpo_lt_withtaxdata.
                clear ls_lfbw.
              endloop.
            endif.

            clear ls_interface01x.
            read table lt_interface01x into ls_interface01x index 1.

            if ls_interface01x-mwskz1 = 'V1' or ls_interface01x-mwskz1 = 'VA'.
              livpo_headerdata-gross_amount = xtotalpo + xtotaladj + ( xtotalpo * ( 10 / 100 ) ).
            elseif ls_interface01x-mwskz1 = 'V2'.
              livpo_headerdata-gross_amount = xtotalpo + xtotaladj + ( xtotalpo * ( 1 / 100 ) ).
            else.
              livpo_headerdata-gross_amount = xtotalpo + xtotaladj.
            endif.

            livpo_headerdata-gross_amount = livpo_headerdata-gross_amount + xtotalppkb.

            if ls_interface01x-waers = 'IDR'.
              livpo_headerdata-gross_amount = round( val = livpo_headerdata-gross_amount dec = 0 mode = 5 ).
            endif.

            refresh livpo_lt_return.
            clear : headerdata_change, headerdata_changex, table_change.
            move-corresponding headerdata_dtl to headerdata_change.

            headerdata_change-pstng_date     = ls_interface01x-budat.
            headerdata_change-bline_date     = ls_interface01x-zfbdt.
            headerdata_change-doc_date       = ls_interface01x-bldat.
            headerdata_change-ref_doc_no     = ls_interface01x-xblnr.
            headerdata_change-currency       = ls_interface01x-waers.
            headerdata_change-gross_amount   = livpo_headerdata-gross_amount.
            headerdata_change-bline_date     = ls_interface01x-zfbdt.
            headerdata_change-pmnttrms       = ls_interface01x-zterm.
            headerdata_change-pmnt_block     = ls_interface01x-zlspr.
            headerdata_change-header_txt     = ls_interface01x-bktxt.
            headerdata_change-alloc_nmbr     = ls_interface01x-zuonr.
            headerdata_change-bus_area       = ls_interface01x-gsber.
            headerdata_change-calc_tax_ind   = 'X'.
            headerdata_change-del_costs_taxc = ls_interface01x-mwskz1.
            headerdata_change-business_place = ls_interface01x-bupla.

            headerdata_changex-pstng_date = 'X'.
            headerdata_changex-bline_date = 'X'.
            headerdata_changex-doc_date   = 'X'.

            headerdata_changex-ref_doc_no   = 'X'.
            headerdata_changex-currency     = 'X'.
            headerdata_changex-gross_amount = 'X'.

            headerdata_changex-bline_date  = 'X'.
            headerdata_changex-pmnttrms    = 'X'.
            headerdata_changex-pmnt_block  = 'X'.
            headerdata_changex-header_txt  = 'X'.
            headerdata_changex-alloc_nmbr  = 'X'.
            headerdata_changex-bus_area    = 'X'.
            headerdata_changex-calc_tax_ind   = 'X'.
            headerdata_changex-del_costs_taxc = 'X'.
            headerdata_changex-business_place = 'X'.

            if livpo_lt_glaccountdata is not initial.
              table_change-glaccountdata    = 'X'.
              table_change-withtaxdata      = 'X'.
            endif.

            call function 'BAPI_INCOMINGINVOICE_CHANGE'
              exporting
                invoicedocnumber   = i_belnr
                fiscalyear         = i_gjahr
                table_change       = table_change
                headerdata_change  = headerdata_change
                headerdata_changex = headerdata_changex
              tables
*               itemdata           = livpo_lt_itemdata
*               accountingdata     = livpo_lt_accountingdata
                glaccountdata      = livpo_lt_glaccountdata
*               taxdata            = livpo_lt_taxdata
                withtaxdata        = livpo_lt_withtaxdata
                return             = livpo_lt_return.

            clear livpo_ls_return.
            read table livpo_lt_return into livpo_ls_return with key type = 'E'.

            if sy-subrc = 0.
              clear : ls_ztvim002, livpo_ls_return.
              read table livpo_lt_return into livpo_ls_return index 1.
              concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
              ls_ztvim002-transtype = 'CRN2'.
              ls_ztvim002-lifnr     = ls_interface01x-lifnr.
              ls_ztvim002-textfile  = ls_dir-name.
              ls_ztvim002-status    = '2'.
              ls_ztvim002-notes     = livpo_ls_return-message.
              ls_ztvim002-refrence  = ls_interface01x-sapdoc.
              ls_ztvim002-notes     = livpo_ls_return-message.
              ls_ztvim002-erdat     = sy-datum.
              ls_ztvim002-itime     = sy-uzeit.
              ls_ztvim002-uname     = sy-uname.
              modify ztvim002 from ls_ztvim002.
            else.
              call function 'BAPI_TRANSACTION_COMMIT'
                exporting
                  wait = 'X'.

              refresh livpo_lt_return.
              call function 'BAPI_INCOMINGINVOICE_POST'
                exporting
                  invoicedocnumber = i_belnr
                  fiscalyear       = i_gjahr
                tables
                  return           = livpo_lt_return.

              if livpo_lt_return is initial.
                call function 'BAPI_TRANSACTION_COMMIT'
                  exporting
                    wait = 'X'.

                "Insert Log History
                clear ls_ztvim002.
                concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
                ls_ztvim002-transtype = 'CRN2'.
                ls_ztvim002-lifnr     = ls_interface01x-lifnr.
                ls_ztvim002-textfile  = ls_dir-name.
                ls_ztvim002-status    = '1'.
                ls_ztvim002-notes     = 'Credit Memo Posted'.
                ls_ztvim002-refrence  = ls_interface01x-sapdoc.
                ls_ztvim002-erdat     = sy-datum.
                ls_ztvim002-itime     = sy-uzeit.
                ls_ztvim002-uname     = sy-uname.
                modify ztvim002 from ls_ztvim002.

                clear xawkey.
                refresh : livpo_lt_rbkp, livpo_lt_rseg, livpo_lt_bkpf, livpo_lt_bseg, livpo_lt_rbco.
                xawkey = ls_interface01x-sapdoc.

                append ls_interface01x to livpo_returntovim-vim_iv[].

                clear ls_interface01x.

                delete dataset file_source.
              else.
                clear livpo_ls_return.
                read table livpo_lt_return into livpo_ls_return with key type = 'E'.
                clear : ls_ztvim002, livpo_ls_return.
                read table livpo_lt_return into livpo_ls_return index 1.

                select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
                if ls_ztvim002-transid is initial.
                  concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
                endif.
                ls_ztvim002-transtype = 'CRN2'.
                ls_ztvim002-lifnr     = ls_interface01x-lifnr.
                ls_ztvim002-textfile  = ls_dir-name.
                ls_ztvim002-status    = '2'.
                ls_ztvim002-notes     = livpo_ls_return-message.
                ls_ztvim002-refrence  = ls_interface01x-sapdoc.
                ls_ztvim002-notes     = livpo_ls_return-message.
                ls_ztvim002-erdat     = sy-datum.
                ls_ztvim002-itime     = sy-uzeit.
                ls_ztvim002-uname     = sy-uname.
                modify ztvim002 from ls_ztvim002.
              endif.
            endif.
          endif.
        endloop.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.
    endif.
  endmethod.                    "readfilecn

  method delt_credit_note.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'CRN3'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'CNCL_CN' and name ns 'CLAIM'.
        refresh : lt_interface01, lt_interface01x.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_interface01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_interface01-invid "START OF LIV HEADER
                                                ls_interface01-lifnr
                                                ls_interface01-sapdoc
*                                                ls_interface01-bukrs
                                                ls_interface01-budat.

                    append ls_interface01 to lt_interface01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_interface01 is not initial.

        clear : livpo_invoicedocnumber, livpo_fiscalyear, ls_interface01.

        read table lt_interface01 into ls_interface01 index 1.
        livpo_invoicedocnumber = ls_interface01-sapdoc(10).
        livpo_fiscalyear       = ls_interface01-sapdoc+10(4).
        refresh livpo_lt_return.
        call function 'BAPI_INCOMINGINVOICE_DELETE'
          exporting
            invoicedocnumber = livpo_invoicedocnumber
            fiscalyear       = livpo_fiscalyear
          tables
            return           = livpo_lt_return.

        clear livpo_ls_return.
        read table livpo_lt_return into livpo_ls_return with key type = 'E'.
        if sy-subrc = 0.
          clear ls_ztvim002.
*          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'CRN3'.
          ls_ztvim002-lifnr     = ls_interface01x-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = livpo_ls_return-message.
          ls_ztvim002-refrence  = ls_interface01-sapdoc.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.
        else.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.

          "Insert Log History
          clear ls_ztvim002.
          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          ls_ztvim002-transtype = 'CRN3'.
          ls_ztvim002-lifnr     = ls_interface01x-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = 'Parked Credit Memo Deleted'.
          ls_ztvim002-refrence  = ls_interface01-sapdoc.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

          delete dataset file_source.
        endif.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.
    endif.
  endmethod.                    "delt_credit_note

  method revs_credit_note.
    clear ls_ztvim001.
    select single * from ztvim001 into ls_ztvim001 where uname = sy-uname and transtype = 'CRN4'.
    if ls_ztvim001 is not initial.
      src_dir = ls_ztvim001-pathfile.
      clear: dir_name.
      dir_name = src_dir.
      refresh t_dir.
      perform get_directory_list  using     dir_name
                                  changing  v_err
                                            t_dir[].
      clear ls_dir.
      refresh : livpo_returntovim-rbkp[], livpo_returntovim-rseg[],livpo_returntovim-vim_iv[].

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIKEY'.
      livpo_returntovim-apikey-apikey = ls_ztvim000-zobjectval.

      clear ls_ztvim000.
      select single * from ztvim000 into ls_ztvim000 where zobject = 'VIM_APIURL'.
      api_url = ls_ztvim000-zobjectval.

      loop at t_dir into ls_dir where name cs 'REVS_CN' and name ns 'CLAIM'.
        refresh : lt_interface01, lt_interface01x.
        clear: file_source, dst_source.
        file_source = |{ src_dir }/{ ls_dir-name }|.
        dst_source  = |{ src_dir }/{ ls_dir-name }|.
        replace all occurrences of 'INPROGRESS' in dst_source with 'PROCESSED'.

        try.
            open dataset file_source for input in text mode encoding default
            ignoring conversion errors message x_msg.

            if sy-subrc ne 0.
            else.
              open dataset dst_source for output in text mode encoding default.
              if sy-subrc = 0.
                do.
                  clear: ls_string, ls_interface01.
                  read dataset file_source into ls_string.
                  if sy-subrc ne 0.
                    exit.
                  else.
                    transfer ls_string to dst_source.
                    split ls_string at tab into ls_interface01-invid "START OF LIV HEADER
                                                ls_interface01-lifnr
                                                ls_interface01-sapdoc
                                                ls_interface01-bukrs
                                                ls_interface01-budat.

                    append ls_interface01 to lt_interface01.
                  endif.
                enddo.
              endif.
            endif.
          catch cx_sy_file_authority.
            x_err_copy = 1. " not_authorized
          catch cx_sy_file_open .
            x_err_copy = 2. " file_open.
        endtry.

        check lt_interface01 is not initial.

        clear : livpo_invoicedocnumber, livpo_fiscalyear, ls_interface01.

        data reasonreversal type  bapi_incinv_fld-reason_rev.
        read table lt_interface01 into ls_interface01 index 1.
        livpo_invoicedocnumber = ls_interface01-sapdoc(10).
        livpo_fiscalyear       = ls_interface01-sapdoc+10(4).
        reasonreversal         = ls_interface01-bukrs.
        refresh livpo_lt_return.

        call function 'BAPI_INCOMINGINVOICE_CANCEL'
          exporting
            invoicedocnumber                = livpo_invoicedocnumber
            fiscalyear                      = livpo_fiscalyear
            reasonreversal                  = reasonreversal
            postingdate                     = ls_interface01-budat
*         IMPORTING
*           INVOICEDOCNUMBER_REVERSAL       =
*           FISCALYEAR_REVERSAL             =
          tables
            return                          = livpo_lt_return.

        clear livpo_ls_return.
        read table livpo_lt_return into livpo_ls_return with key type = 'E'.

        if sy-subrc = 0.
          clear ls_ztvim002.
*          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          select single transid from ztvim002 into ls_ztvim002-transid where textfile  = ls_dir-name.
          if ls_ztvim002-transid is initial.
            concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          endif.
          ls_ztvim002-transtype = 'CRN4'.
          ls_ztvim002-lifnr     = ls_interface01x-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '2'.
          ls_ztvim002-notes     = livpo_ls_return-message.
          concatenate livpo_invoicedocnumber livpo_fiscalyear into ls_ztvim002-refrence.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.
        else.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.

          "Insert Log History
          clear ls_ztvim002.
          concatenate sy-datum sy-uzeit into ls_ztvim002-transid.
          ls_ztvim002-transtype = 'CRN4'.
          ls_ztvim002-lifnr     = ls_interface01x-lifnr.
          ls_ztvim002-textfile  = ls_dir-name.
          ls_ztvim002-status    = '1'.
          ls_ztvim002-notes     = 'Credit Memo Reversed'.
          concatenate livpo_invoicedocnumber livpo_fiscalyear into ls_ztvim002-refrence.
          ls_ztvim002-erdat     = sy-datum.
          ls_ztvim002-itime     = sy-uzeit.
          ls_ztvim002-uname     = sy-uname.
          modify ztvim002 from ls_ztvim002.

          data xawkey type awkey.
          clear xawkey.
          refresh : livpo_lt_rbkp, livpo_lt_rseg, livpo_lt_bkpf, livpo_lt_bseg, livpo_lt_rbco.
          xawkey = ls_interface01x-sapdoc.

*          append ls_interface01x to livpo_returntovim-vim_iv[].
*
*          select * from rbkp into corresponding fields of table livpo_lt_rbkp
*            where belnr = livpo_invoicedocnumber and gjahr = livpo_fiscalyear
*                  and lifnr = ls_interface01x-lifnr.
*
*          select * from rseg into corresponding fields of table livpo_lt_rseg
*            where belnr = livpo_invoicedocnumber and gjahr = livpo_fiscalyear.
*
*          select * from rbco into corresponding fields of table livpo_lt_rbco
*            where belnr = livpo_invoicedocnumber and gjahr = livpo_fiscalyear.
*
*          select * from bkpf into corresponding fields of table livpo_lt_bkpf
*            where awkey = xawkey.
*
*          if livpo_lt_bkpf is not initial.
*            select * from bseg into corresponding fields of table livpo_lt_bseg
*              for all entries in livpo_lt_bkpf
*                where bukrs = livpo_lt_bkpf-bukrs and belnr = livpo_lt_bkpf-belnr and gjahr = livpo_lt_bkpf-gjahr.
*          endif.
*
*          append lines of livpo_lt_rbkp to livpo_returntovim-rbkp[].
*          append lines of livpo_lt_rseg to livpo_returntovim-rseg[].
*          append lines of livpo_lt_bkpf to livpo_returntovim-bkpf[].
*          append lines of livpo_lt_bseg to livpo_returntovim-bseg[].
*          append lines of livpo_lt_rbco to livpo_returntovim-rbco[].

          delete dataset file_source.
        endif.

        close dataset file_source.
        close dataset dst_source.
        clear ls_dir.
      endloop.

*      if livpo_returntovim is not initial.
*        create object o_json.
*        o_json->lowercase_names      = abap_true.
*        o_json->include_empty_values = abap_true.
*        o_json->pretty_print         = abap_true.
*
*        concatenate api_url '/interface/livpo' into api_url.
*
*        api_data = o_json->encode( livpo_returntovim ).
*        call function 'Z_VIM_CALL_API'
*          exporting
*            api_data    = api_data
*            api_url     = api_url
*            api_method  = 'POST'
*          importing
*            lv_response = lv_response.
*      endif.
    endif.
  endmethod.                    "revs_credit_note

  method display.
    data is_layout_lvc  type  lvc_s_layo.
    refresh lt_ztvim002.
    select * from ztvim002 into table lt_ztvim002
      where uname = sy-uname and erdat = sy-datum.

    sort lt_ztvim002 by transid descending.

    clear is_layout_lvc.
    is_layout_lvc-cwidth_opt = 'X'.
    is_layout_lvc-zebra      = 'X'.

    call function 'REUSE_ALV_GRID_DISPLAY_LVC'
      exporting
        i_callback_program = sy-repid
        i_structure_name   = 'ZTVIM002'
        is_layout_lvc	     = is_layout_lvc
        i_save             = 'X'
      tables
        t_outtab           = lt_ztvim002
      exceptions
        program_error      = 1
        others             = 2.
  endmethod.                    "display

  method run_handler.
    call method timer->run.
    call method cl_gui_cfw=>set_new_ok_code
      exporting
        new_code = 'REFR'.

    g_ref_vim002_event->readfilerfq( ).
    g_ref_vim002_event->park_liv_po( ).
    g_ref_vim002_event->delt_liv_po( ).
    g_ref_vim002_event->post_liv_po( ).
    g_ref_vim002_event->cncl_liv_po( ).

    g_ref_vim002_event->park_liv_claim( ).
    g_ref_vim002_event->post_liv_claim( ).
    g_ref_vim002_event->delt_liv_claim( ).
    g_ref_vim002_event->revs_liv_claim( ).

    g_ref_vim002_event->park_liv_npo( ).
    g_ref_vim002_event->post_liv_npo( ).
    g_ref_vim002_event->revs_liv_npo( ).
    g_ref_vim002_event->delt_liv_npo( ).

    g_ref_vim002_event->park_dp_po( ).
    g_ref_vim002_event->revs_dp_po( ).

    g_ref_vim002_event->park_dp_npo( ).
    g_ref_vim002_event->delt_dp_npo( ).
    g_ref_vim002_event->post_dp_npo( ).
    g_ref_vim002_event->revs_dp_npo( ).

    g_ref_vim002_event->park_credit_note( ).
    g_ref_vim002_event->delt_credit_note( ).
    g_ref_vim002_event->post_credit_note( ).
    g_ref_vim002_event->revs_credit_note( ).

*    g_ref_vim002_event->display( ).

  endmethod.                    "RUN_HANDLER
endclass.                    "LCL_VIM002_EVENT IMPLEMENTATION
