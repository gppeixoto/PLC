package lista3;
import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class TwitchPlays {
	
	private static char[][] caverna;
	private static AtomicBoolean modo;
    private static AtomicInteger posX;
    private static AtomicInteger posY;
    static Red red;
    private static int n;
    private static AtomicInteger countUp;
    private static AtomicInteger countDown;
    private static AtomicInteger countLeft;
    private static AtomicInteger countRight;
    private static AtomicInteger countAnar;
    private static AtomicInteger countDemo;
    private Lock lock = new ReentrantLock();
    private static Random gerador = new Random(); 
    
    public TwitchPlays(int num){
    	n = num;
		countUp = new AtomicInteger(0);
		countDown = new AtomicInteger(0);
		countLeft = new AtomicInteger(0);
		countRight = new AtomicInteger(0);
		countAnar = new AtomicInteger(0);
		countDemo = new AtomicInteger(0);
		modo = new AtomicBoolean(true);
		posX = new AtomicInteger(0);
		posY = new AtomicInteger(0);
		red = new Red(); 
    	gerarCaverna();
    }
	
	public static void gerarCaverna(){
		caverna = new char[n][n];
		
		boolean temp;
		int saidaX, saidaY, redX, redY;
		
		for (int i = 0; i < n; i++) {
			for(int j = 0; j < n; j++){
				temp = gerador.nextBoolean();
				if (temp) caverna[i][j] = '_';
				else caverna[i][j] = 'Z';
			}
		}
		
		redX = gerador.nextInt(n);
		redY = gerador.nextInt(n);	
		posX.set(redX);
		posY.set(redY);
		caverna[redX][redY] = 'R';
		
		if(n > 1) {
			do {
				saidaX = gerador.nextInt(n);
				saidaY = gerador.nextInt(n);
			} while (saidaX == redX && saidaY == redY);

			caverna[saidaX][saidaY] = 'X';
		}
		else if(n == 1) {
			red.ganhou = true;
		}
	}
	
	public synchronized void print(){

		for (int i = 0; i < n; i++) {
			for(int j = 0; j < n; j++){
				System.out.print(caverna[i][j] + " ");
				if (j == n - 1) System.out.println();
			}
		}
		
	}
	
	public void jogar(){
		int func;
		
		func = gerador.nextInt(6);
		
		if(countAnar.get() == 100 || countDemo.get() == 100) {
			if(countAnar.get() == 100) {
				modo.set(true);
			}
			else{
				modo.set(false);
			}
			
			countUp.set(0);
			countDown.set(0);
			countLeft.set(0);
			countRight.set(0);
			countAnar.set(0);
			countDemo.set(0);
		}
		
		if(func == 4) countAnar.incrementAndGet();
		else if (func ==5) countDemo.incrementAndGet();
		else {
			if(!modo.get()) {		
			
					if(func == 0) countUp.incrementAndGet();
					else if(func == 1) countDown.incrementAndGet();
					else if(func == 2) countLeft.incrementAndGet();
					else if(func == 3) countRight.incrementAndGet();
					
					if(countUp.get() == 100 || countDown.get() == 100 || countLeft.get() == 100 || countRight.get() == 100) {
						
						while (!lock.tryLock()) Thread.yield();
						
						try {
						caverna[posX.get()][posY.get()] = '_';

						if(countUp.get() == 100 && (posX.get() - 1) >= 0) {
							posX.decrementAndGet();
						}
						else if(countDown.get() == 100 && (posX.get() + 1) < n) {
							posX.incrementAndGet();
						}
						else if(countLeft.get() == 100 && (posY.get() - 1) >= 0) {
							posY.decrementAndGet();
						}
						else if (countRight.get() == 100 && (posY.get() + 1) < n){
							posY.incrementAndGet();
						}

						if(caverna[posX.get()][posY.get()] == 'X') {
							red.ganhou = true;
							caverna[posX.get()][posY.get()] = 'R';
							return;
						}
						else if(caverna[posX.get()][posY.get()] == 'Z') red.achouZubat();
						caverna[posX.get()][posY.get()] = 'R';

						countUp.set(0);
						countDown.set(0);
						countLeft.set(0);
						countRight.set(0);
						} finally {
							lock.unlock();
						}
					}
			}
			else {

				if(!lock.tryLock() || red.perdeu || red.ganhou) {
					return;
				}
				else {
					try{

						caverna[posX.get()][posY.get()] = '_';

						if(func == 0 && (posX.get() - 1) >= 0) {
							posX.decrementAndGet();
						}
						else if(func == 1 && (posX.get() + 1) < n) {
							posX.incrementAndGet();
						}
						else if(func == 2 &&(posY.get() - 1) >= 0) {
							posY.decrementAndGet();
						}
						else if (func == 3 && (posY.get() + 1) < n){
							posY.incrementAndGet();
						}

						if(caverna[posX.get()][posY.get()] == 'X') {
							red.ganhou = true;
							caverna[posX.get()][posY.get()] = 'R';
							return;
						}	
						else if(caverna[posX.get()][posY.get()] == 'Z') red.achouZubat();

						caverna[posX.get()][posY.get()] = 'R';

						countUp.set(0);
						countDown.set(0);
						countLeft.set(0);
						countRight.set(0);
					} finally {
						lock.unlock();
					}
				}
			}

		}
	}

	public static void main(String[] args) {
		Scanner in = new Scanner(System.in); 
		
		System.out.println("Insira o tamanho (N) da caverna (NxN):");
		int tamanho = in.nextInt();
		
		TwitchPlays tp = new TwitchPlays(tamanho);
		
		System.out.println("Escolha o modo do jogo: anarquia(1) ou democracia(2)");
		int mode = in.nextInt();
		
		if(mode == 1) modo.set(true);
		else modo.set(false);
		ArrayList<Jogador> jogadores = new ArrayList<Jogador>();
		
		tp.print();
		
		System.out.println("Insira o numero de jogadores:");
		int qtd = in.nextInt();
		
		for(int i = 0; i < qtd; i++){
			jogadores.add(new Jogador(tp));
		}
		
		for(int i = 0; i < qtd; i++){
			jogadores.get(i).start();
		}
		
		for(int i = 0; i < qtd; i++){
			try {
				jogadores.get(i).join();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	       
		tp.print();
		if(tp.red.ganhou) System.out.println("Saiu da caverna!");
		else System.out.println("Perdeu todos os pokemons! :(");
	}

}

class Red {
	ArrayList<Integer> pokemons = new ArrayList<Integer>();
	Random gerador = new Random(); 
	boolean perdeu;
	boolean ganhou;
	
	public Red(){
		this.perdeu = false;
		this.ganhou = false;
		int hp;
		
		for(int j = 0; j < 3; j++){
			hp = gerador.nextInt(21);
			pokemons.add(hp + 30);
		}		
	}
	
	public void achouZubat(){
		int poke = gerador.nextInt(pokemons.size());
		int dano = gerador.nextInt(6) + 5;
		pokemons.set(poke, pokemons.get(poke) - dano);

		if(pokemons.get(poke) <= 0) {
			pokemons.remove(poke);
		}
			
		if(pokemons.size() == 0){
			perdeu = true;
		}
	}
}

class Jogador extends Thread{
	TwitchPlays ger;
    
    public Jogador(TwitchPlays ger){
        this.ger = ger;
    }
    
    public void run(){
        while(!ger.red.perdeu && !ger.red.ganhou){
            ger.jogar();
        }

    }
}