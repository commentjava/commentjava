class c {
        void m() { 
                new Object() {
                        void foo() { System.out.println("foo"); }
                }.foo();
        }
}
