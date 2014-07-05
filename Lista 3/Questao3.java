import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.Scanner;
import java.util.concurrent.locks.Condition;
import java.util.ArrayList;

/**
 * Questão 3 da Lista 3 de PLC 2014.1
 * July 3th 2014
 * @author Guilherme Peixoto
 */

class Pergunta {
	String perg;
	int time;
	public Pergunta(){this.perg = null; this.time=0;}
	public Pergunta(int time){this.perg = "Me dá meio ponto?"; this.time = time;}
	public Pergunta(String perg, int time){this.perg = perg; this.time=time;}
}

class SafeList {
	ArrayList<Pergunta> pergs;
	Lock l;
	Condition c;
	int max;
	int n;
	boolean isFull;
	boolean isEmpty;
	int timeEncher;
	int timeBeber;
	int total;
	int sofar;
	
	/**
	 * 
	 * @param max Número max de perguntas que podem ser memorizadas.
	 * @param agua Tempo que leva para encher a caneca
	 * @param bebe Tempo que leva para beber
	 * @param total Total de perguntas que serão respondidas.
	 */
	public SafeList(int max, int agua, int bebe, int total){
		this.pergs = new ArrayList<Pergunta>();
		this.l = new ReentrantLock();
		this.c = l.newCondition();
		this.max = max;
		this.isFull = false;
		this.isEmpty = true;
		this.timeEncher = agua;
		this.timeBeber = bebe;
		this.total = total;
		this.sofar = 0;
	}
	
	/**
	 * Aluno irá por perguntas
	 * @param
	 */
	public void prod(Pergunta p){
		l.lock();
		try {
			while (isFull)
				try {
					c.await();
				} catch (InterruptedException ie) {}
			System.out.println("Vou perguntar '" + p.perg + "' e vai levar " + p.time);
			pergs.add(p);
			++n;
			//System.out.println("prod: " + n + " " + max + " " + pergs.size());
			if (n==max) isFull = true;
			if (pergs.size()==1) c.signalAll();
		} finally {
			l.unlock();
		}
	}
	
	/**
	 * Professor vai ler perguntas.
	 * Não pode ler se não houver perguntas, precisa esperar que algum aluno
	 * faça perguntas.
	 */
	public void cons(){
		l.lock();
		try {
			while (pergs.isEmpty()){
				try {
					c.await();
				} catch (InterruptedException e) {}
			}
			if (sofar % 2 == 0 && sofar > 0){
				try {
					System.out.println("Respondi duas pergs, vou encher a caneca e volto em" + timeEncher);
					Thread.sleep(timeEncher);
				} catch (InterruptedException ie) {}
			}
			Pergunta aux = pergs.remove(0);
			System.out.println("Vou responder: " + aux.perg);
			try {
				System.out.println("Vou beber água por " + timeBeber + " e responder em " + aux.time);
				Thread.sleep(aux.time + timeBeber);
			} catch (InterruptedException e) {}
			++sofar;
			--n;
			//System.out.println("cons: " + n + " " + sofar + " " + max);
			if (n < max) {isFull=false; c.signalAll();}
		} finally {
			l.unlock();
		}
	}
}

class Aluno extends Thread {
	int id;
	ArrayList<Pergunta> pergs;
	SafeList l;
	
	Aluno(int id, SafeList l, ArrayList<Pergunta> pergs){
		this.pergs = pergs;
		this.id = id; 
		this.l = l;
		}
	
	public void run() {
		while (!pergs.isEmpty()){
			l.prod(pergs.remove(0));
		}
	}
}

class Professor extends Thread {
	int id;
	SafeList l;
	Professor(int id, SafeList l){this.id = id; this.l = l;}
	
	public void run() {
		while (l.sofar < l.total){
			l.cons();
		}
	}
}

public class Questao3 {
	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		int numAlunos, tCaneca, tBeber, maxPergs;
		System.out.println("Digite respectivamente:");
		System.out.println("1) numAlunos \n2) tCaneca \n3) tBeber \n4) maxPergs");
		numAlunos = in.nextInt();
		tCaneca = in.nextInt(); 
		tBeber = in.nextInt();
		maxPergs = in.nextInt();
		
		final SafeList l = new SafeList(numAlunos, tCaneca, tBeber, maxPergs);
		ArrayList<Aluno> alunos = new ArrayList<Aluno>();
		ArrayList<Pergunta> auxPergs;
		int nPergs, timePerg; String perg; Scanner str = new Scanner(System.in);
		for (int i=0; i<numAlunos; ++i){
			System.out.println("Aluno " + i + ") Quantas perguntas: ");
			nPergs = in.nextInt();
			auxPergs = new ArrayList<Pergunta>();
			for (int j=0; j<nPergs; ++j){
				System.out.println("Tempo da pergunta: ");
				timePerg = in.nextInt();
				System.out.println("Pergunta: ");
				perg = str.nextLine();
				auxPergs.add(new Pergunta(perg, timePerg));
			}
			alunos.add(new Aluno(i, l, auxPergs));
		}
		/*
		for (int i=0; i<400; ++i){
			auxPergs = new ArrayList<Pergunta>();
			for (int j=0; j<3; ++j){
				auxPergs.add(new Pergunta(100));
			}
			alunos.add(new Aluno((i+1), l, auxPergs));
		}
		 */
		str.close();
		in.close();
		Long begin = System.currentTimeMillis();
		Professor prof = new Professor(40, l);
		for (int i=0; i<numAlunos; ++i) alunos.get(i).start();
		prof.start();
		try {
			prof.join();
		} catch (InterruptedException ie){}
		for (int i=0; i<numAlunos; ++i){
			try {
				alunos.get(i).join();
			} catch(InterruptedException ie) {}
		}
		Long end = System.currentTimeMillis();
		System.out.println("Time elapsed: " + (end-begin)/1000.0);
		
	}
}
