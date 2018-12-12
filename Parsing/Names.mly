%%

(* 6.5 *)
%public package_name:
  | identifier { Tree("package_name", [$1]) }
  | package_name PERIOD identifier { Tree("package_name", [$1; $3]) }

%public type_name:
  | identifier { Tree("type_name", [$1]) }
  | package_or_type_name PERIOD identifier { Tree("type_name", [$1; $3])  }

%public expression_name:
  | identifier { Tree("expression_name", [$1]) }
  | ambiguous_name PERIOD identifier { Tree("expression_name", [$1; $3]) }

%public method_name:
  | identifier { Tree("method_name", [$1]) }
  | ambiguous_name PERIOD identifier { Tree("method_name", [$1; $3]) }

package_or_type_name:
  | identifier { Tree("package_or_type_name", [$1]) }
  | package_or_type_name PERIOD identifier { Tree("package_or_type_name", [$1; $3]) }

ambiguous_name:
  | identifier { Tree("ambiguous_name", [$1]) }
  | ambiguous_name PERIOD identifier { Tree("ambiguous_name", [$1; $3]) }
