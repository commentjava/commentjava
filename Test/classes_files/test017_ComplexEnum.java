@annotation
enum SecondEnum {
        VAR,
        LAB,
}

@annotation
enum SecondEnum {
        VAR,
        LAB,
        ;
        int i = j;
}

@annotation
enum NoTrailingComma implements Cat {
        VAR,
        LAB
        ;
        int i = j;
}

@annotation(coucou)
public enum NoConsts implements Cat, tata {
        ;
        int i = j;
}
