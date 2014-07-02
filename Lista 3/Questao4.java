/*
O n�mero m�ximo de Threads no meu PC (realmente paralelas) s�o quatro, uma
vez que � quad-core. Todos os testes (excetos os sequenciais) foram feitos
utilizando 4 threads.

O "log" a seguir segue a seguinte regra:
n m 	//n e m usados, respectivamente
problema divisiveis / execucao sequencial
problema divisiveis / execucao parelala - divisao estatica
problema divisiveis / execucao parelala - divisao dinamica
problema coprimos / execucao sequencial
problema coprimos / execucao paralela - divisao estatica
problema coprimos / execucao paralela - divisao dinamica

A seguir, o resultado dos testes:

7 100
Tempo de execu��o: 0.004s
Tempo de execu��o: 0.003s
Tempo de execu��o: 0.011s

Tempo de execu��o: 0.001s
Tempo de execu��o: 0.001s
Tempo de execu��o: 0.002s

50 1000
Tempo de execu��o: 0.002s
Tempo de execu��o: 0.012s
Tempo de execu��o: 0.007s

Tempo de execu��o: 0.001s
Tempo de execu��o: 0.003s
Tempo de execu��o: 0.003s

1000 125000
Tempo de execu��o: 0.011s
Tempo de execu��o: 0.025s
Tempo de execu��o: 0.041s

Tempo de execu��o: 0.023s
Tempo de execu��o: 2.368s
Tempo de execu��o: 2.392s


137 150000
Tempo de execu��o: 0.02s
Tempo de execu��o: 0.037s
Tempo de execu��o: 0.059s

Tempo de execu��o: 0.29s
Tempo de execu��o: 27.906s
Tempo de execu��o: 22.385s

12345 400000
Tempo de execu��o: 0.027s
Tempo de execu��o: 0.034s
Tempo de execu��o: 0.014s

Tempo de execu��o: 0.064s
Tempo de execu��o: 51.778s
Tempo de execu��o: 51.032s

53579 100000
Tempo de execu��o: 0.002s
Tempo de execu��o: 0.007s
Tempo de execu��o: 0.011s

Tempo de execu��o: 0.031s
Tempo de execu��o: 9.317s
Tempo de execu��o: 9.329s


Podemos perceber que ao trabalhar com n�mero pequenos, a performance da execu��o
paralela � an�loga � sequencial, tendo apenas um gargalo na inicializa��o das threads
praticamente. Conforme vai aumentando o n�mero, a execu��o paralela torna-se cada
vez mais eficiente devido ao atraso causado pelos blocos sincronizados. � not�vel
tamb�m que a execu��o � mais demorada ao tentar resolver o problema dos n�meros coprimos,
uma vez que esse problema tem uma complexidade algor�timica superior e exige o uso
de MDC para fazer a computa��o do n�mero.

Podemos ver notavelmente principalmente no teste do problema dos n�meros coprimos,
em especial ao lidar com n�mero maiores. Nesse problema espec�fico, ao lidar com
blocos sincronizados como estrat�gia de paraleliza��o, � prefer�vel - como pudemos
verificar - utilizar a abordagem puramente sequencial ao resolver esse problema.

*/

import java.util.ArrayList;
import java.util.Scanner;
import java.util.concurrent.atomic.AtomicInteger;

class Node {
	int val;
	Node next;

	Node (int val){
		this.val = val;
		this.next = null;
	}
}

class SafeList {
	Node head;

	SafeList(){
		this.head = null;
	}

	public synchronized boolean empty(){
		return (head==null);
	}

	public synchronized void add (int val){
		Node novo = new Node(val);
		if (head == null) { //lista vazia
			this.head = novo;
		} else {
			if (head.next == null) {
				head.next = novo;
			} else {
				Node aux = this.head;
				while (aux.next!=null){
					aux = aux.next;
				}
				aux.next = novo;
			}
		}
	}

	public synchronized void print(){
		Node aux = this.head;
		while (aux!=null){ System.out.print(aux.val + " "); aux=aux.next;}
		System.out.println();
	}
}


class SafeCounter {
	private AtomicInteger c;

	public SafeCounter() {
		c = new AtomicInteger(1);
	}

	public void inc(){
		c.incrementAndGet();
	}

	public int get(){
		return c.get();
	}
}

class DivisaoDinamica implements Runnable {
	private SafeList l;// = new SafeList();
	private SafeCounter c;
	private int n;
	private int m;
	public DivisaoDinamica(int n, int m, SafeList l, SafeCounter c) {
		this.n = n;
		this.m = m;
		this.l = l;
		this.c = c;
	}

	public void run() {
		while (true){
			if (c.get() > this.m) break;
			if (c.get() % this.n == 0) {
				l.add(c.get());
			}
			c.inc();
		}
	}
}

class DivisaoEstatica implements Runnable {
	private SafeList l; //trecho compartilhado
	private int init;
	private int end;
	private int n;

	/**
	 * Classe que implementa o trabalho feito pela thread por divis�o est�tica.
	 * @param init index inicial que a thread ir� iniciar a percorrer o array
	 * @param end index final que a thread ir� parar o trabalho
	 */
	public DivisaoEstatica(int init, int end, int n, SafeList l) {
		this.init = init; this.setEnd(end); this.n = n; this.l = l;
	}

	public void run() {
		for (int i=this.init; i<this.getEnd(); ++i){
			if (i%this.n==0) l.add(i);
		}
	}

	public int getEnd() {
		return end;
	}

	public void setEnd(int end) {
		this.end = end;
	}
}

class DivisaoEstaticaB implements Runnable{
	private SafeList l; //trecho compartilhado
	private int init;
	private int end;
	private int n;

	/**
	 * Classe que implementa o trabalho feito pela thread por divis�o est�tica.
	 * @param init index inicial que a thread ir� iniciar a percorrer o array
	 * @param end index final que a thread ir� parar o trabalho
	 */

	public boolean isCoPrime(int a, int b){
		return mdc(a,b)==1;
	}

	public int mdc(int a, int b){
		if (a==0) return b;
		while (b!=0){
			if (a>b) a-=b;
			else b-=a;
		}
		return a;
	}

	public DivisaoEstaticaB(int init, int end, int n, SafeList l) {
		this.init = init; this.end = end; this.n = n; this.l = l;
	}
	public void run() {
		for (int i=this.init; i<this.end; ++i){
			if (isCoPrime(i, this.n)) l.add(i);
		}
	}
	
	public void setEnd(int end){
		this.end = end;
	}
}

class DivisaoDinamicaB implements Runnable {
	private SafeList l;
	private SafeCounter c;
	private int n;
	private int m;
	public DivisaoDinamicaB(int n, int m, SafeList l, SafeCounter c) {
		this.n = n;
		this.m = m;
		this.l = l;
		this.c = c;
	}
	
	public boolean isCoPrime(int a, int b){
		return mdc(a,b)==1;
	}
	
	public int mdc(int a, int b){
		if (a==0) return b;
		while (b!=0){
			if (a>b) a-=b;
			else b-=a;
		}
		return a;
	}
	
	public void run() {
		while (true){
			if (c.get() > this.m) break;
			if (isCoPrime(c.get(), this.n)) {
				l.add(c.get());
			}
			c.inc();
		}
	}
}

public class Questao4 {

	public static boolean isCoPrime(int a, int b){
		return mdc(a,b)==1;
	}

	public static int mdc(int a, int b){
		if (a==0) return b;
		while (b!=0){
			if (a>b) a-=b;
			else b-=a;
		}
		return a;
	}

	public static void main(String[] args) {
		final SafeList l = new SafeList();
		final SafeCounter c = new SafeCounter();

		Scanner in = new Scanner(System.in);
		System.out.println("Escolha sua op��o:");
		System.out.println("1- divisiveis / sequencial");
		System.out.println("2- divisiveis / divisao estatica");
		System.out.println("3- divisiveis / divisao dinamica");
		System.out.println("4- coprimos / sequencial");
		System.out.println("5- coprimos / divisao estatica");
		System.out.println("6- coprimos / divisao dinamica");
		int problema = in.nextInt();
		System.out.println("digite n, m, numero de threads: ");
		int n = in.nextInt(); int m = in.nextInt(); int nt = in.nextInt();
		in.close();
		Long begin = System.currentTimeMillis();
		switch (problema){
		case (1):
			ArrayList<Integer> numbers = new ArrayList<Integer>();
			for (int i=1; i<m; ++i){
				if (i%n==0) numbers.add(i);
			}
			//Descomente a linha abaixo se quiser saber os n�meros divis�veis
			//for (int i=0; i<numbers.size(); ++i) System.out.print(numbers.get(i) + " "); System.out.println();
			break;
		case (2):
			int ws = m/nt;//window size
			DivisaoEstatica[] de = new DivisaoEstatica[nt];
			int aux = 1;
			for (int i=0; i<nt; ++i){
				de[i] = new DivisaoEstatica(aux, aux+ws-1, n, l);
				aux+=ws;
			}
			de[nt-1].setEnd(m+1); //ultima thread fica com o restante
			try {
				for (int i=0; i<nt; ++i) de[i].run();
			} finally {
				//Descomente a linha abaixo se quiser saber os n�meros
				//l.print();
			}
			break;
		case (3):
			DivisaoDinamica[] dd = new DivisaoDinamica[nt];
			for (int i=0; i<nt; ++i) dd[i] = new DivisaoDinamica(n, m, l, c);
			try {
				for (int i=0; i<nt; ++i) dd[i].run();
			} finally {
				//Descomente a linha abaixo se quiser saber os n�meros
				//l.print();
			}
			break;
		case (4):
			ArrayList<Integer> numbers2 = new ArrayList<Integer>();
			for (int i=1; i<=m; ++i){
				if (isCoPrime(i, n)) numbers2.add(i);
			}
			//Descomente a linha abaixo se quiser saber os n�meros divis�veis
			//for (int i=0; i<numbers2.size(); ++i) System.out.print(numbers2.get(i) + " "); System.out.println();
		break;
		case (5):
			int ws2 = m/nt;//window size
			DivisaoEstaticaB[] de2 = new DivisaoEstaticaB[nt];
			int aux2 = 1;
			for (int i=0; i<nt; ++i){
				de2[i] = new DivisaoEstaticaB(aux2, aux2+ws2-1, n, l);
				aux2+=ws2;
			}
			de2[nt-1].setEnd(m+1); //ultima thread fica com o restante
			try {
				for (int i=0; i<nt; ++i) de2[i].run();
			} finally {
				//Descomente a linha abaixo se quiser saber os n�meros
				//l.print();
			}
		break;
		case (6):
			DivisaoDinamicaB[] db2 = new DivisaoDinamicaB[nt];
			for (int i=0; i<nt; ++i){
				db2[i] = new DivisaoDinamicaB(n, m, l, c);
			}
			try {
				for (int i=0; i<nt; ++i) db2[i].run();
			} finally {
				//Descomente a linha abaixo se quiser saber os n�meros
				//l.print();
			}
			break;
		}
		Long end = System.currentTimeMillis();
		System.out.println("Tempo de execu��o: " + (end-begin)/1000.0 + "s");
	}
}
