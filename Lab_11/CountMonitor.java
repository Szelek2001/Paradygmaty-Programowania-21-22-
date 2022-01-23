//Zadanie 2A
class IntCellSyn {
    private int n = 0;
    private boolean block = true;
    synchronized public int getN() {
        while(!block) {
            try {
                wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        block = false;
        notifyAll();

        return n;
    }

    synchronized void setN(int n) {

        while(block) {
            try {
                wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        block = true;
        notifyAll();

        this.n = n;
    }
}

class CountMonitor extends Thread {
    private static IntCellSyn n = new IntCellSyn();

    @Override public void run() {
        int temp;
        for (int i = 0; i < 200000; i++) {
            temp = n.getN();
            n.setN(temp + 1);
        }
    }

    public static void main(String[] args) {
        CountMonitor p = new CountMonitor();
        CountMonitor q = new CountMonitor();
        p.start();
        q.start();

        try { p.join(); q.join(); }
        catch (InterruptedException e) { }
        System.out.println("The value of n is " + n.getN());
    }
}