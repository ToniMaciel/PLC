// José Antônio Alves Maciel (jaam)

import java.util.Scanner;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class JoseAntonioAlvesMaciel_q1 {

    public static Lock lock = new ReentrantLock();
    public static Condition chairsOperation = lock.newCondition();
    public static AtomicBoolean chairsInUse = new AtomicBoolean();
    public static AtomicInteger chairPosition = new AtomicInteger();

    public static AtomicInteger[] chairs;

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        System.out.print("Informe a quantidade de jogadores: ");

        chairs = new AtomicInteger[in.nextInt()];
        in.close();

        for (int i = 0; i < chairs.length; i++) {
            chairs[i] = new AtomicInteger(i+1);
        }

        newRound();
        while(chairs.length > 1){
            newRound();
        }

        try {
            Thread.sleep(10);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        System.out.println("O jogador " + chairs[0].get() + " foi o vencedor!");
    }

    private static void newRound() {
        chairPosition = new AtomicInteger(0);

        AtomicInteger chairsAux[] = new AtomicInteger[chairs.length];
        for (int i = 0; i < chairsAux.length; i++) {
            chairsAux[i] = new AtomicInteger(chairs[i].get());
        }

        chairs = new AtomicInteger[chairsAux.length - 1];
        for (int i = 0; i < chairs.length; i++) {
            chairs[i] = new AtomicInteger(0);
        }

        for (int i = 0; i < chairsAux.length; i++) {
            new Player(chairsAux[i].get()).start();
        }
    }
    
    public static class Player extends Thread{
        public int id;

        public Player(int id){
            this.id = id;
        }

        @Override
        public void run() {
            try {
                lock.lock();
                
                while(chairsInUse.get()){
                    chairsOperation.await();
                }
                
                chairsInUse.set(true);

                if((chairPosition.get() < chairs.length) && (chairs[chairPosition.get()].get() == 0) && (this.id != 0)){
                    chairs[chairPosition.get()].set(this.id);
                    chairPosition.getAndIncrement();
                } else if (this.id != 0){
                    System.out.println("O jogador " + this.id + " foi eliminado");
                }

                chairsInUse.set(false);
                chairsOperation.signalAll();
            } catch (InterruptedException e) {
                e.printStackTrace();
            } finally {
                lock.unlock();
            }
        }
    }
}
