{
    super.myMethod();
    super.<T1>myMethod();
    super.<T1, T2>myMethod();
    super.myMethod(a1, a2);
    super.<T1, T2>myMethod(a1, true, 1);
    Class.OtherClass.super.myMethod();
    Class.OtherClass.super.<T1, T2>myMethod();
    Class.OtherClass.super.myMethod(a1, a2, 3, true);
    Class.OtherClass.super.<T1, T2>myMethod(a1, a2<<1, 3>4, true);
    //TODO: accept this case super.<T1, myType<t1, t2, ? extends t3>>myMethod(a1, true, 1);
    //WHERE >> is recognized as a token
}