{
    a = new Test();
    a = new <myType> Test();
    a = new Test(3);
    a = new TestBody() {
        int laba = 12;
    };
    a = new <myType> Test(3);
    a = new <myType> TestBody() {
        int laba = 12;
    };
    a = new TestBody(3) {
        int laba = 12;
    };
    a = new <myType> TestBody(3) {
        int laba = 12;
    };
    a = this.new <myType> c();
    // a = this.new c <myType> ();
    a = this.new TestBody() {
        int laba = 12;
    };
    // a = this.new <myType> c <myType> ();
    a = this.new <myType> c(3);
    a = this.new <myType> c() {
        int laba = 12;
    };
    a = this.new c(3) {
        int laba = 12;
    };
    a = this.new <myType> c(3) {
        int laba = 12;
    };
}