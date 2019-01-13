public class Foo {
   public void doSomething() {
      System.out.print("Hello ");

      class Bar {
         Bar() {
            System.out.println("World");
         }
      }

      new Bar();
   }

   public static void main(String[] args) {
      new Foo().doSomething();
   }
}
