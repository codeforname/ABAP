REPORT zexec_time_example.

INCLUDE zcl_exec_time.

START-OF-SELECTION.

  zcl_exec_time=>obj( )->start( 'write_100' ).

  do 100 times.
    WRITE: 'code under measure'.
  ENDDO.

  zcl_exec_time=>obj( )->stop( 'write_100' ).
