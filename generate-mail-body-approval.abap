*&---------------------------------------------------------------------*
*& Report YMAIL03
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ymail04.

DATA:
  lv_subject            TYPE so_obj_des,
  lv_attachment_subject TYPE sood-objdes,
  ls_mailto             TYPE zsmail_to,
  lt_mailto             TYPE TABLE OF zsmail_to,
  lt_otf                TYPE TABLE OF itcoo,
  it_soli               TYPE soli_tab,
  ls_soli               TYPE soli,
  i_mail_sender         TYPE  ad_smtpadr,
  i_mail_sender_name    TYPE  ad_smtpadr,
  e_status              TYPE char50,
  e_sent_to_all         TYPE  os_boolean.

DATA:
  lt_ekko TYPE TABLE OF ekko,
  lt_ekpo TYPE TABLE OF ekpo.

DATA : lt_data TYPE TABLE OF x255.
DATA : wa_line TYPE x255.

DATA: lo_docking_container TYPE REF TO cl_gui_docking_container.
DATA: lo_html    TYPE REF TO cl_gui_html_viewer.
DATA: lv_url TYPE char255.
DATA: ok_code(20).

DATA filename TYPE string.

DATA: it_solix  TYPE solix_tab,
      lt_hex    TYPE solix_tab,
      ls_hex    TYPE solix,
      lv_size   TYPE i,
      file_type TYPE so_obj_tp.

DATA lt_mail_content  TYPE  soli_tab.
DATA ls_mail_content  TYPE  soli.

DATA: bin_file TYPE string .

PARAMETERS p_ebeln TYPE ebeln.
PARAMETERS fname TYPE localfile.
PARAMETERS proot TYPE zchar50 DEFAULT '/usr/sap/attachment/'.
PARAMETERS fltype TYPE char3.

** Selection screen value request
AT SELECTION-SCREEN ON VALUE-REQUEST FOR fname.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      static    = 'X'
    CHANGING
      file_name = fname.

START-OF-SELECTION.

  CLEAR file_type.
  file_type  = fltype.
  lv_subject = |Approve PO { p_ebeln }|.
  filename   = proot.
  filename   = |{ filename }{ fname }|.

  PERFORM read_file.
  PERFORM set_mail_body.
  PERFORM send_email.

FORM read_file.
  fname = filename.
  OPEN DATASET fname FOR INPUT IN BINARY MODE.
  IF sy-subrc = 0.
    DO.
      CLEAR wa_line.
      READ DATASET fname INTO wa_line.
      IF sy-subrc = 0.
        APPEND wa_line TO lt_data.
      ELSE.
        APPEND wa_line TO lt_data.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "read_file

FORM set_mail_body.

  REFRESH lt_ekpo.
  SELECT * FROM ekpo
    INTO TABLE lt_ekpo
      WHERE ebeln = p_ebeln.

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
  CONCATENATE 'Mohon untuk review dan approve/reject PO Berikut' '' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  "Start Table
  CLEAR ls_soli.
  CONCATENATE '<table border="1">' '' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR : ls_soli.
  CONCATENATE '<thead><th>Nomor PO</th>' '' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  CONCATENATE '' '<th>Material</th>' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  CONCATENATE ls_soli '<th>Description</th>' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  CONCATENATE ls_soli '<th style="text-align:right;">Quantity</th><th>Action</th></thead>' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  ls_soli = '<tbody>'.
  APPEND ls_soli TO it_soli.
*  CLEAR ls_data.

  DATA lv_qty TYPE char30.
  LOOP AT lt_ekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>).
    CLEAR : ls_soli.
    CONCATENATE '<tr>' ' ' INTO ls_soli.
    APPEND ls_soli TO it_soli.

    CLEAR : ls_soli.
    CONCATENATE '<td>' <fs_ekpo>-ebeln '</td>' INTO ls_soli.
    APPEND ls_soli TO it_soli.

    <fs_ekpo>-matnr = |{ <fs_ekpo>-matnr ALPHA = OUT }|.
    CLEAR : ls_soli.
    CONCATENATE '<td>' <fs_ekpo>-matnr '</td>' INTO ls_soli.
    APPEND ls_soli TO it_soli.

    CLEAR : ls_soli.
    CONCATENATE '<td>' <fs_ekpo>-txz01 '</td>' INTO ls_soli.
    APPEND ls_soli TO it_soli.

    CLEAR lv_qty.
    WRITE <fs_ekpo>-menge TO lv_qty UNIT <fs_ekpo>-meins.
    CLEAR : ls_soli.
    <fs_ekpo>-meins = |{ <fs_ekpo>-meins ALPHA = OUT }|.
    CONCATENATE '<td style="text-align:right;">' lv_qty ' - ' <fs_ekpo>-meins '</td>' INTO ls_soli.
    APPEND ls_soli TO it_soli.

*    clear lv_url.
*    lv_url = |<a >|.
    CLEAR : ls_soli.
    CONCATENATE '<td style="text-align:center;">' '' INTO ls_soli.
*    CONCATENATE '<td>' lv_url '</td>' INTO ls_soli.
    APPEND ls_soli TO it_soli.

*    CLEAR : ls_soli.
*    CONCATENATE '' '<input name="approve-text" id="approve-text" placeholder="Approval Note"/>' INTO ls_soli.
*    APPEND ls_soli TO it_soli.

*    CLEAR : ls_soli.
*    CONCATENATE '' '<button class="button" type="button" onclick="approveDoc(' <fs_ekpo>-ebeln ')">APPROVE</button>' INTO ls_soli.
*    APPEND ls_soli TO it_soli.
*
*    CLEAR : ls_soli.
*    CONCATENATE '<t/d>' '' INTO ls_soli.
*    APPEND ls_soli TO it_soli.

    CLEAR : ls_soli.
    CONCATENATE '<a href="' '' INTO ls_soli.
*    CONCATENATE '<td>' lv_url '</td>' INTO ls_soli.
    APPEND ls_soli TO it_soli.

    CLEAR : lv_url, ls_soli.
    lv_url = |http://domain-name/sap/bc/zva_srv/approvePo?sap-client=000&sap-user=userid&sap-password=password&ponumber={ <fs_ekpo>-ebeln }&appcode=A001|.
    ls_soli = lv_url.
    APPEND ls_soli TO it_soli.

    CLEAR : ls_soli.
    CONCATENATE '" target="_blank" class="button" style="color:white;">Approve</a>' '</td>' INTO ls_soli.
    APPEND ls_soli TO it_soli.

    CLEAR : ls_soli.
    CONCATENATE '</tr>' ' ' INTO ls_soli.
    APPEND ls_soli TO it_soli.
  ENDLOOP.

  CLEAR ls_soli.
  ls_soli = '</tbody>'.
  APPEND ls_soli TO it_soli.

  CLEAR ls_soli.
  ls_soli = '</table><br>'.
  APPEND ls_soli TO it_soli.
  "End of table

  CLEAR ls_soli.
  CONCATENATE 'Terima Kasih' '' INTO ls_soli.
  APPEND ls_soli TO it_soli.

  "End of Html Body Tag
  CLEAR ls_soli.
  ls_soli = '</body></html>'.
  APPEND ls_soli TO it_soli.
ENDFORM.

FORM send_email.
  IF lt_data IS INITIAL.
    EXIT.
  ENDIF.
  DATA lt_pdf              TYPE w3mimetabtype.
  DATA ls_pdf              TYPE w3mime.

  DATA lv_filesize TYPE w3param-cont_len.

  DATA ls_pdf_string_x     TYPE xstring.


  REFRESH : lt_pdf, lt_hex.
  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    CLEAR : ls_pdf, ls_hex.
    ls_hex-line = <fs_data>.
    APPEND ls_hex TO lt_hex.

    ls_pdf-line = <fs_data>.
    APPEND ls_pdf TO lt_pdf.
  ENDLOOP.

  REFRESH lt_mailto.
  ls_mailto-recipient_type = 'TO'.
  ls_mailto-email_address  = 'receiver@gmail.com'.
  APPEND ls_mailto TO lt_mailto.

  REFRESH lt_mail_content.
  CLEAR ls_mail_content.
  ls_mail_content-line = 'Testing Email'.
  APPEND ls_mail_content TO lt_mail_content.

  i_mail_sender = 'sender@email.co.id'.
  i_mail_sender_name = 'SAP S4 Hana Mail System'.
*  BREAK-POINT.
  CALL FUNCTION 'YFM_SEND_EMAIL'
    EXPORTING
      i_mail_subject     = lv_subject
*     I_ATTACHMENT_SUBJECT       =
      lt_mail_content    = it_soli
*     I_MAIL_SUBJECT2    =
      i_mail_sender      = i_mail_sender
      i_mail_sender_name = i_mail_sender_name
      lt_hex             = lt_hex
      i_file_type        = file_type
    IMPORTING
      e_status           = e_status
      e_sent_to_all      = e_sent_to_all
    TABLES
*     lt_otf             =
      lt_mailto          = lt_mailto
*     lt_excel           =
    .

  WRITE e_status.
ENDFORM.
