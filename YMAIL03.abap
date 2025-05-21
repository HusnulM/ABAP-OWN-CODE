*&---------------------------------------------------------------------*
*& Report YMAIL03
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ymail03.

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

DATA : lt_data TYPE TABLE OF x255.
DATA : wa_line TYPE x255.

DATA: lo_docking_container TYPE REF TO cl_gui_docking_container.
DATA: lo_html    TYPE REF TO cl_gui_html_viewer.
DATA: lv_url TYPE char255.
DATA: ok_code(20).

DATA filename TYPE string.

DATA: it_solix TYPE solix_tab,
      lt_hex   TYPE solix_tab,
      ls_hex   TYPE solix,
      lv_size  TYPE i.

DATA lt_mail_content  TYPE  soli_tab.
DATA ls_mail_content  TYPE  soli.

DATA: bin_file TYPE string .

PARAMETERS fname TYPE localfile.
PARAMETERS proot TYPE zchar50 DEFAULT '/usr/sap/attachment/'.

** Selection screen value request
AT SELECTION-SCREEN ON VALUE-REQUEST FOR fname.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      static    = 'X'
    CHANGING
      file_name = fname.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

START-OF-SELECTION.

  lv_subject = fname.
  filename  = proot.
  filename  = |{ filename }{ fname }|.

  PERFORM read_file.
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

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = ls_pdf_string_x
    TABLES
      binary_tab = lt_pdf.

  DATA: gv_base64 TYPE string.
  DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring( it_solix = lt_pdf ).
  DATA(lv_len)      = xstrlen( lv_bin_data ).

  lv_filesize = xstrlen( lv_bin_data ).

  CALL FUNCTION 'SSFC_BASE64_ENCODE'
    EXPORTING
      bindata = lv_bin_data "gv_string
      binleng = lv_filesize
    IMPORTING
      b64data = gv_base64.


  REFRESH lt_mailto.
  ls_mailto-recipient_type = 'TO'.
  ls_mailto-email_address  = 'testmail@gmail.com'.
  APPEND ls_mailto TO lt_mailto.

  REFRESH lt_mail_content.
  CLEAR ls_mail_content.
  ls_mail_content-line = 'Testing Email'.
  APPEND ls_mail_content TO lt_mail_content.

  i_mail_sender = 'sender@mail.co.id'.
  i_mail_sender_name = 'SAP S4 Hana Mail System'.

  CALL FUNCTION 'YFM_SEND_EMAIL'
    EXPORTING
      i_mail_subject     = lv_subject
*     I_ATTACHMENT_SUBJECT       =
      lt_mail_content    = lt_mail_content
*     I_MAIL_SUBJECT2    =
      i_mail_sender      = i_mail_sender
      i_mail_sender_name = i_mail_sender_name
      lt_hex             = lt_hex
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
