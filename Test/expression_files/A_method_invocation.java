{
    a = myMethod();
    b = myMethod(a1, a2);
    myMethod();
    myMethod(a1, a2);
    qualifier.myMethod();
    qualifier.anotherQualifier.myMethod().myMethod();
    System.out.println("myString");
    getClass().<T1, T2>myMethod();
    getClass().myMethod(a1, a2);
    getClass().myMethod(a1, a2=1, a3=getValue(true));
    getClass().<T1, T2>myMethod(a1, a2);
    myName.<T1, T2>myMethod();
    myName.<T1, T2>myMethod(a1, a2, a=myFunc());
}