public class UnknownMethodOfInstance {
    public void someMethod() {
        Ext ext = new Ext();
        ext.nonExistingMethod();
    }
}

public class Ext {
}