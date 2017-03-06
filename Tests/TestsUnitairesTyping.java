public class A {
    int attr;
    public void m1() { }

    public void m2() {
        int a = 1;
        true && false;
        return;
        Ext ext = new Ext();
        ext.mext();
        { Ext ext2 = new Ext(); }
        boolean c = true || false;
        if (c) {
            ext.mext();
        } else {
            m1();
        }
        int result = c ? 1 : 2;
        if (result instanceof String) {
            result = 4;
        }
        for (int i = 0; i<10; i++) {i=5;}
    }
}

public class Ext {
    public int mext() {}
}