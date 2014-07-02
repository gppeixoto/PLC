import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import q1.PF_Blocos;

/*
##################################################################
########################## README ################################
##################################################################

---- As estratégias adotadas em cada operação estão explicadas----
---- uma única vez em cada método referente ao travamento     ----
---- utilizando blocos sincronizados. Como a estratégia é     ----
---- análoga utilizando locks, não comentei nessas operações, ----
---- visto que seriam praticamente os mesmos comentários.     ----

##################################################################
######################### OBRIGADO! ##############################
##################################################################

*/
class No<T> {
	T val;
	No<T> next;
	No<T> previous;

	No (T val){
		this.val = val;
		this.next = null;
		this.previous = null;
	}
	
	No(){
		this.val = null;
		this.next = null;
		this.previous = null;
	}
}

class PF_Blocos<T> {
	private No<T> head;
	private No<T> tail;
	private Object ch_tail; //Monitor usado para travar tail
	private Object ch_head; //Monitor usado para travar head
	private AtomicInteger size;

	public PF_Blocos(){
		this.head = new No<T>();
		this.tail = new No<T>();
		this.ch_tail = new Object();
		this.ch_head = new Object();
		this.size = new AtomicInteger();
		this.size.set(0);
	}
	
	/**
	 * Se a PilaFilha possui menos de dois elementos, então a fila possui
	 * null ou apenas head==tail. Nesse caso, o acesso fica em zona de exclusão
	 * mútua por meio de blocos sincronizados. Caso contrário, é possível
	 * realizar a operação queue_push e dequeue simultaneamente. Não é possível
	 * executar simultaneamente duas operações de queue_push, nem é possível
	 * executar queue_push e pop ao mesmo tempo, pois operaria na mesma área
	 * de memória.
	 * @param val
	 */
	public void queue_push(T val){
		synchronized (ch_tail) {
			if (this.size.get() < 2) {
				//ZONA DE EXCLUSÃO MÚTUA
				synchronized (ch_head) {
					if (this.size.get() == 0) {
						No<T> novo = new No<T>(val);
						this.head = novo;
						this.tail = novo;
						this.size.incrementAndGet();
					} else {
						No<T> novo = new No<T>(val);
						this.head.next = this.tail = novo;
						this.tail.previous = this.head;
						this.size.incrementAndGet();
					}
				}
				//ZONA DE EXCLUSÃO MÚTUA
			} else {
				No<T> novo = new No<T>(val);
				this.tail.next = novo;
				this.tail = novo;
				this.tail.previous = this.tail;
				this.size.incrementAndGet();
			}
		}
	}
	
	/**
	 * Se a PilaFilha possui menos de dois elementos, então a fila possui
	 * null ou apenas head==tail. Nesse caso, o acesso fica em zona de exclusão
	 * mútua por meio de blocos sincronizados. Caso contrário, é possível
	 * realizar a operação queue_push e dequeue simultaneamente OU pop
	 * e dequeue simultaneamente. Não é possível, entretanto, executar
	 * simultaneamente duas operações de dequeue.
	 */
	public void dequeue(){
		synchronized (ch_head) {
			if (this.size.get() < 2){ 
				//ZONA DE EXCLUSÃO MÚTUA
				if (this.size.get() == 0){
					return;
				} else {
					this.head = this.tail = null;
					this.size.decrementAndGet();
				}
				//ZONA DE EXCLUSÃO MÚTUA
			} else {
				this.head = this.head.next;
				this.head.previous = null;
				this.size.decrementAndGet();
			}
		}
	}

	/**
	 * Se a PilaFilha possui menos de dois elementos, então a fila possui
	 * null ou apenas head==tail. Nesse caso, o acesso fica em zona de exclusão
	 * mútua por meio de blocos sincronizados. Caso contrário, é possível
	 * realizar a operação pop e dequeue simultaneamente. Não é possível, entretanto, 
	 * executar simultaneamente duas operações de pop (ou pop e queue_push), pois iria 
	 * operar sobre a mesma área de memória.
	 */
	public void pop(){
		synchronized (ch_tail) {
			if (this.size.get() < 2){
				//ZONA DE EXCLUSÃO MÚTUA
				if (this.size.get() == 0){
					return;
				} else {
					this.head = this.tail = null;
					this.size.decrementAndGet();
				}
				//ZONA DE EXCLUSÃO MÚTUA
			} else {
				this.tail = this.tail.previous;
				this.tail.next = null;
				this.size.decrementAndGet();
			}
		}
	}
}

class PF_Travas<T> {
	private Lock l1 = new ReentrantLock();
	private Lock l2 = new ReentrantLock();
	private Lock l3 = new ReentrantLock();
	private AtomicInteger size;
	private No<T> head;
	private No<T> tail;

	public PF_Travas() {
		this.head = this.tail = null;
		this.size = new AtomicInteger();
		this.size.set(0);
	}

	public void queue_push(T val){
		this.l1.lock();
		try{
			No<T> novo = new No<T>(val);
			if (this.size.get() == 0){
				this.l2.lock();
				try{
					this.head = novo;
					this.tail = this.head;
					size.getAndIncrement();
				} finally {
					this.l2.unlock();
				}
			} else if (this.size.get() == 1){
				this.l2.lock();
				try {
					this.head.next = novo;
					this.tail = this.head.next;
					this.size.getAndIncrement();
				} finally {
					this.l2.unlock();
				}
			} else {
				No<T> aux = this.head;
				while (aux != this.tail) aux = aux.next;
				aux.next = novo;
				this.tail = aux.next;
				this.size.getAndIncrement();
			}
		} finally {
			this.l1.unlock();
		}
	}

	public void pop(){
		this.l1.lock();
		try {
			if (this.size.get() == 0){
				this.l2.lock();
				try {
					return;
				} finally {
					this.l2.unlock();
				}
			} else if (this.size.get() == 1){
				this.l2.lock();
				try{
					this.head = this.tail = null;
					this.size.getAndDecrement();
				} finally{
					this.l2.unlock();
				}
			} else {
				No<T> aux = this.head;
				while (aux.next != this.tail) aux = aux.next;
				this.tail = aux;
				this.tail.next = null;
				this.size.getAndDecrement();
			}
		} finally {
			this.l1.unlock();
		}
	}

	public void dequeue(){
		this.l3.lock();
		try {
			if (this.size.get() == 0){
				this.l2.lock();
				try{
					return;
				} finally{
					this.l2.unlock();
				}
			}
			else if (this.size.get() == 1){
				this.l2.lock();
				try {
					this.head = this.tail = null;
					this.size.getAndDecrement();
				} finally {
					this.l2.unlock();
				}
			} else {
				this.head = this.head.next;
				this.size.getAndDecrement();
			}
		} finally {
			this.l3.unlock();
		}
	}
}

public class Questao1 {
//	public static void main(String[] args) {
//		final PF_Blocos<Integer> p = new PF_Blocos<Integer>(); //teste com blocos sync
//		//final PF_Travas<Integer> p = new PF_Travas<Integer>(); //teste com locks
//		Runnable r1 = new Runnable() {
//			public void run() {
//				p.queue_push(1);
//				p.queue_push(4);
//				p.queue_push(8);
//				p.dequeue();
//			}};
//
//		Runnable r2 = new Runnable() {
//			public void run() {
//				p.queue_push(7);
//				p.queue_push(2);
//				p.pop();
//				p.queue_push(5);
//			}};
//
//		Runnable r3 = new Runnable() {
//			public void run() {
//				p.queue_push(3);
//				p.queue_push(6);
//				p.queue_push(9);
//				p.dequeue();
//				p.queue_push(10);
//			}};
//
//		Runnable r4 = new Runnable() {
//			public void run() {
//				p.queue_push(1);
//				p.queue_push(2);
//				p.queue_push(3);
//				p.dequeue();
//				p.queue_push(4);
//				p.queue_push(5);
//			}};
//
//		Runnable r5 = new Runnable(){
//			public void run() {
//				p.queue_push(3);
//				p.queue_push(5);
//				p.queue_push(7);
//				p.pop();
//				p.dequeue();
//			}};
//
//		Runnable r6 = new Runnable(){
//			public void run() {
//				p.queue_push(4);
//				p.queue_push(6);
//				p.queue_push(7);
//				p.pop();
//				p.dequeue();
//			}};
//
//		Thread t1 = new Thread(r1); Thread t2 = new Thread(r2);
//		Thread t3 = new Thread(r3); Thread t4 = new Thread(r4);
//		Thread tt = new Thread(r5);
//		Thread tt2 = new Thread(r6);
//		t1.start(); t2.start(); t3.start(); t4.start();
//		tt.start(); tt2.start();
//		try {
//			tt.join();
//			tt2.join();
//			t1.join(); t2.join(); t3.join();
//			t4.join();
//		} catch (InterruptedException te) {}
}
