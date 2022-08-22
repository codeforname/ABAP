REPORT ztext_example.

INCLUDE zcl_text.

START-OF-SELECTION.

  DATA: lr_text TYPE REF TO zcl_text,
        lt_string TYPE STANDARD TABLE OF string,
        lv_text TYPE string.

  APPEND 'zpiop_091|003|local text: &1|l' TO lt_string.
  CREATE OBJECT lr_text.
  lr_text->set_local_text( lt_string ).

  lv_text = lr_text->text( iv_repid = sy-repid iv_lang = sy-langu iv_key = '003' iv_msgv1 = 'Yupii' ).
  WRITE: lv_text.
