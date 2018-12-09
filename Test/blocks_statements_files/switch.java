{
    switch (true) {}

    switch (true) {
        case myVar: {
            ;
        }
        case myVar2: {
            ;
        }
        default: {
            ;
        }
    }

    switch (true) {
        case myVar:
        case myVar2:
        default:
    }

    switch (true) {
        case myVar: {
            ;
        }
        case myVar2: {
            ;
        }
        default: {
            ;
        }
        case myVar3:
        case myVar4:
    }
}

switch_statement:
  | SWITCH L_PAR expression R_PAR switch_block { "switch(" ^ $3 ^ ")(" ^ $5 ^ ")" }

switch_block:
  | L_BRACE R_BRACE { "{}" }
  | L_BRACE switch_block_statement_groups R_BRACE { "{" ^ $2 ^ "}" }
  | L_BRACE switch_labels R_BRACE { "{" ^ $2 ^ "}" }
  | L_BRACE switch_block_statement_groups switch_labels R_BRACE { "{" ^ $2 ^ $3 ^ "}"  }

switch_block_statement_groups:
  | switch_block_statement_group { $1 }
  | switch_block_statement_groups switch_block_statement_group { $1 ^ $2 }

switch_block_statement_group:
  | switch_labels block_statements { $1 ^ $2 }

switch_labels:
  | switch_label { $1 }
  | switch_labels switch_label { $1 ^ $2 }

switch_label:
  | CASE constant_expression COLON { "case(" ^ $2 ^ "): " }
  | CASE enum_constant_name %prec COLON { "case(" ^ $2 ^ "): "}
  | DEFAULT COLON { "default:" }

enum_constant_name:
  | identifier { $1 }