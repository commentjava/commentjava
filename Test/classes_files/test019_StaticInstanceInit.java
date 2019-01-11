class simpleStatic {
        static {
                cubfunction.call();
        }
}

class multipleStatic {
        static {
                second.call();
        }
        static {
                cubfunction.method.twice(arg);
                int i = 3;
        }
}

class mixed {
        static {
                second.call();
        }
        {
                cubfunction.method.twice(arg);
        }
}

public class mixed2 extends father {
        static {
                second.call();
        }
        {
                cubfunction.method.twice(arg);
        }
}
