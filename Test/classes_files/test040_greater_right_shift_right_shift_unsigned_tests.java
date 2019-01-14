/* type parameters */
class firstClass<A extends secondClass> {}

class firstClass<A extends secondClass<?>> {}

/* non_wild_type_arguments */

class firstClass{
    public firstClass(){
        <secondClass<thirdClass>>super();
    }
}

class firstClass{
    public firstClass(){
        <secondClass<thirdClass<fourthClass>>>super();
    }
}

class firstClass{
    public firstClass(){
        <secondClass<thirdClass<fourthClass<fithClass>>>>super();
    }
}

/* type_parameters */

class firstClass{
    <thirdClass extends sixthClass & fourthClass<? extends fithClass<?>>>secondClass my_method();
}

class firstClass{
    <thirdClass extends fourthClass<? extends fithClass<?>>>secondClass my_method();
}

class firstClass{
    <thirdClass extends fourthClass<fithClass>>secondClass my_method();
}
