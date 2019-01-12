{
    for (int i: arrayOfInt) {
        System.out.println(i);
    }

    for (int i: arrayOfInt) System.out.println(i);

    for (int i: arrayOfInt)
        System.out.println(i);

    for(Number n: numbers) {
        n++;
    }

    for (final @annotation int n: number) n++;

    for (final @annotation(e="E") @annotation(e="E") mType<t1, t2, ? extends type> n: number) n++;
}