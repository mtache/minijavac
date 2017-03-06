public class A {
    int attr;
    public void m1() {}

    public void m2() {
        short sh = 3;
        byte by = 5;
        float someCalc = sh + 1.0 + 3 + by << 5;
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
    }
}

public class Ext {
    public int mext() {}
}