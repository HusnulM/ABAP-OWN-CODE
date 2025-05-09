*&---------------------------------------------------------------------*
*& Report YMAIL02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ymail02.
TABLES: rbkp.

DATA: ls_rbkp TYPE rbkp,
      lt_rbkp TYPE TABLE OF rbkp.

DATA:
  lv_subject  TYPE so_obj_des,
  lv_subject2 TYPE string,
  lt_mailto   TYPE TABLE OF zsmail_to,
  ls_mailto   TYPE zsmail_to,
  it_soli     TYPE soli_tab,
  ls_soli     TYPE soli.

SELECT-OPTIONS:
  s_belnr FOR rbkp-belnr,
  s_gjahr FOR rbkp-gjahr.

START-OF-SELECTION.
  PERFORM get_data.
  IF lt_rbkp IS NOT INITIAL.
    PERFORM set_mail_content.
    PERFORM send_email.
  ENDIF.

FORM get_data.
  REFRESH lt_rbkp.
  SELECT * FROM rbkp
    INTO TABLE lt_rbkp
      WHERE belnr IN s_belnr AND
            gjahr IN s_gjahr.
ENDFORM.

FORM set_mail_content.
  REFRESH it_soli.
  CLEAR ls_soli.
  ls_soli = '<html xmlns="http://www.w3.org/1999/xhtml" xmlns:xfa="http://www.xfa.org/schema/xma-template/2.1"><head>'.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
*  CONCATENATE '<style>' '' INTO ls_soli.
  CONCATENATE '<style>' 'table {font-family: arial, sans-serif;border-collapse: collapse;width: 100%;}' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  CONCATENATE 'tr:nth-child(even) {background-color: #dddddd;}' '' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  CONCATENATE '.button {background-color: #008CBA;border: none;color:white;padding:' '' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  CONCATENATE '15px 32px;text-align: center;text-decoration: none;' '' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  CONCATENATE 'display: inline-block;font-size: 10px;margin: 4px 2px;cursor: pointer;}' '</style>' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  ls_soli = '</head><body>'.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  DATA lv_str TYPE string.
  lv_str = |Yth. Bapak/Ibu, <br><br>|.
*  lv_str = |Yth. Bapak/Ibu, { lv_cname }<br><br>|.
  ls_soli = lv_str.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  CONCATENATE 'Please kindly review and approve the voucher(s) with the Purchase Order below' '' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  "Start Table
  CLEAR ls_soli.
  CONCATENATE '<table border="1">' '' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR : ls_soli.
  CONCATENATE '<thead><th style="text-align:center;">Invoice Number</th>' '' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  CONCATENATE '' '<th style="text-align:right;">Invoice Amount</th>' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  CONCATENATE ls_soli '<th style="text-align:center;">Invoice Date</th>' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  CONCATENATE ls_soli '<th style="text-align:center;">Invoice Note</th></thead>' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  ls_soli = '<tbody>'.
  APPEND ls_soli TO it_soli.
*  CLEAR ls_data.

  DATA :
    lv_netwr TYPE netwr,
    lv_ivval TYPE char30,
    lv_month TYPE char30.

  CLEAR ls_rbkp.
  LOOP AT lt_rbkp INTO ls_rbkp.

    CLEAR : ls_soli.
    CONCATENATE '<tr>' ' ' INTO ls_soli.
    APPEND ls_soli TO it_soli.

    CLEAR : ls_soli.
    CONCATENATE '<td style="text-align:left;">' ls_rbkp-belnr '</td>' INTO ls_soli.
    APPEND ls_soli TO it_soli.

    CLEAR :lv_netwr, lv_ivval.
    lv_netwr = ls_rbkp-rmwwr.
    WRITE lv_netwr TO lv_ivval CURRENCY ls_rbkp-waers.

    CLEAR : ls_soli.
    CONCATENATE '<td style="text-align:right;">' lv_ivval '</td>' INTO ls_soli.
    APPEND ls_soli TO it_soli.

    lv_month = |{ ls_rbkp-budat+6(2) }.{ ls_rbkp-budat+4(2) }.{ ls_rbkp-budat(4) }|.

    CLEAR : ls_soli.
    CONCATENATE '<td style="text-align:center;">' lv_month '</td>' INTO ls_soli.
    APPEND ls_soli TO it_soli.

    CLEAR : ls_soli.
    CONCATENATE '<td style="text-align:left;">' ls_rbkp-sgtxt '</td>' INTO ls_soli.
    APPEND ls_soli TO it_soli.

    CLEAR ls_rbkp.
  ENDLOOP.

  CLEAR ls_soli.
  ls_soli = '</tbody>'.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  ls_soli = '</table><br>'.
  APPEND ls_soli TO it_soli.
  "End of table

  "End of Html Body Tag
  CLEAR ls_soli.
  ls_soli = '</body></html>'.
  APPEND ls_soli TO it_soli.
ENDFORM.

FORM send_email.
  REFRESH lt_mailto.
  CLEAR ls_mailto.
  ls_mailto-email_address  = 'husnulmub@gmail.com'.
  ls_mailto-recipient_type = 'TO'.
  APPEND ls_mailto TO lt_mailto.

  lv_subject = |Approve Invoice { sy-datum } { sy-uzeit }|.

  CALL FUNCTION 'ZFM_SEND_EMAIL'
    EXPORTING
      i_mail_subject  = lv_subject
      lt_mail_content = it_soli
      i_mail_subject2 = lv_subject2
    TABLES
      lt_mailto       = lt_mailto.
ENDFORM.
