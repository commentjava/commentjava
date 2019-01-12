{
    try {
        a = 2;
    } catch (Exception e) {
    }

    try {
        a = 2;
    } finally {
        b = 2;
    }

    try {
        a = 2;
    } catch (Exception e) {
        c = 2;  
    } finally {
        b = 2;
    }

    try {
        a = 2;
    } catch (MyException e) {
        c = 2;  
    } catch (Exception e) {
        c = 2;  
    } finally {
        b = 2;
    }
}