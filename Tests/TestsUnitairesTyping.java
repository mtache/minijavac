public class A {
    int attr;
    public int m1() { int r = 2; return r;}

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
        final int v = 19;
	    for (a; a > v ; a--) { }
        boolean b = true;
        A ref;
        attr = 3;
        ref.attr = 4;
        this.attr = 4;
        while (b) { b = b || false; }

    }
}

public class Ext {
    public int mext() {}
}