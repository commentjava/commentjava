public class Foo<T> {
    public <T> Foo(T param) {
        System.out.println(param.getClass());
    }

    public static void main(String[] args) {
        //Foo<String> foo = new <String>Foo<>("Test");
        Foo<Integer> foo = new <String>Foo<Integer>("Test");
    }
}
