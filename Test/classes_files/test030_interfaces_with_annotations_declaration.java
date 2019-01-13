interface myInterface{
    /* abstract method declaration */
    int myMethod(char a, strangeType b);
    
    /* constant declaration */
    int k = 63;
    strangeType b;

    /* class declaration */
    class myClass implements myInterface{}

    /* interface declaration */
    interface secondInterface{}

    /* annotation type declaration */
    @interface myInterface {
        int id();
        @thisIsAninterface strangeType k();
        public char w();
    }

    @annot
    int annotatedMethod();
}
