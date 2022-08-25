REPORT zcl_mail_test.

INCLUDE zcl_mail.

START-OF-SELECTION.

  DATA: lr_mail TYPE REF TO lcl_mail,
        lt_content TYPE STANDARD TABLE OF char50.

  CREATE OBJECT lr_mail.

  APPEND |This is body of the mail| TO lt_content.
  APPEND |Table lt_content can be any string based table| TO lt_content.

  lr_mail->set_subject( 'This is test' )->add_recipient( sy-uname )->add_content( lt_content )->send( ).
