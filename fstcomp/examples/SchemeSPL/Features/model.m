model : regsim [eceval] operations* [compiler] [documentation] :: main ;
operations : op_basic | op_aritmetic | op_comparison | op_increment ;


%%

compiler implies eceval;
op_basic implies eceval;
op_aritmetic implies eceval;
op_comparison implies eceval;
op_increment implies eceval;

##
eceval { disp="interpreter" }
