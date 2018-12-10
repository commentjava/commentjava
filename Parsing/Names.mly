%%

(* 6.5 *)
%public package_name:
  | i=identifier { i }
  | n=package_name PERIOD i=identifier { n ^ "." ^ i }

%public type_name:
  | i=identifier {Tree("type_name", [Leaf(i)])}
  | n=package_or_type_name PERIOD i=identifier { Tree("type_name", [n; Leaf(i)])  }

%public expression_name:
  | i=identifier { i }
  | n=ambiguous_name PERIOD i=identifier { n ^ "." ^ i }

%public method_name:
  | i=identifier { i }
  | n=ambiguous_name PERIOD i=identifier { n ^ "." ^ i }

package_or_type_name:
  | i=identifier {Tree("type_name", [Leaf(i)])}
  | n=package_or_type_name PERIOD i=identifier { Tree("type_name", [n; Leaf(i)])  } 

ambiguous_name:
  | i=identifier { i }
  | n=ambiguous_name PERIOD i=identifier { n ^ "." ^ i }