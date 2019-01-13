/* no explicit constructor invocation */
class firstClass{
    public firstClass(){
        int var = 23;
    }
}

/* constructor invocation with this */
class firstClass{
    public firstClass(char a){
        char myA = a;
    }
    public firstClass(){
        this();
        int var = 23;
    }
}

/* constructor invocation with this with arguments */
class firstClass{
    public firstClass(char a){
        char myA = a;
    }
    public firstClass(){
        this("e");
        int var = 23;
    }
}

/* constructor invocation with super */
class firstClass{
    public firstClass(){
        super();
        int var = 23;
    }
}

/* constructor invocation with super with arguments */
class firstClass{
    public firstClass(){
        super("e");
        int var = 23;
    }
}

/* constructor invocation with this with nwta */
class firstClass{
    public firstClass(char a){
        char myA = a;
    }
    public firstClass(){
        <strangeType, float[]>this();
        int var = 23;
    }
}

/* constructor invocation with this with arguments with nwta */
class firstClass{
    public firstClass(char a){
        char myA = a;
    }
    public firstClass(){
        <strangeType, float[]>this("e");
        int var = 23;
    }
}

/* constructor invocation with super with nwta */
class firstClass{
    public firstClass(){
        <strangeType, float[]>super();
        int var = 23;
    }
}

/* constructor invocation with super with arguments with nwta */
class firstClass{
    public firstClass(){
        <strangeType, float[]>super("e");
        int var = 23;
    }
}

/* constructor invocation with super with nwta with primary */
class firstClass{
    public firstClass(){
        (new otherClass()).super();
        int var = 23;
    }
}

/* constructor invocation with super with arguments with nwta with primary */
class firstClass{
    public firstClass(){
        (new otherClass()).super("e");
        int var = 23;
    }
}


/* constructor invocation with super with primary */
class firstClass{
    public firstClass(){
        (new otherClass()).<strangeType, float[]>super();
        int var = 23;
    }
}

/* constructor invocation with super with arguments with primary */
class firstClass{
    public firstClass(){
        (new otherClass()).<strangeType, float[]>super("e");
        int var = 23;
    }
}

