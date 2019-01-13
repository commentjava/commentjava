class firstClass extends parentClass {
    public firstClass(int a, strangeType b, Object... c){
        <strangeType>super(a, b); /* doesnt work with <int> */
        int var = 23;
    }
}
