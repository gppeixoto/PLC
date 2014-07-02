package lista3;
import java.util.ArrayList;
import java.util.concurrent.locks.*;
import java.util.Scanner;

/*
Entrada:
ta tempo da agua
te tempo pra encher
N número alunos
n número de perguntas do aluno x
x
y
z
... tempo de cada pergunta
n Número de perguntas do aluno y
mesma coisa até pegar os N alunos
 */

class Gerenciador {
    
    private Lock lock = new ReentrantLock();
    private Condition cond = lock.newCondition();
    private ArrayList<Pergunta> perguntas = new ArrayList<Pergunta>();
    private int n;
    Scanner in = new Scanner(System.in);
    int tempoBeber;
    int tempoEncher;
    int totalPergs;
    int qtdRespondida = 0;
    
    public Gerenciador (int n, int tempoBeber, int tempoEncher, int totalPergs){
        this.n = n;
        this.tempoBeber = tempoBeber;
        this.tempoEncher = tempoEncher;
        this.totalPergs = totalPergs;
    }
    
    public void perguntar(Pergunta p){
        lock.lock();
        try{
            while(perguntas.size() == this.n){
                try{
                    cond.await();
                } catch (InterruptedException e) {}
            }
            
            perguntas.add(p);
            //System.out.println("Pergunta '" + p.texto + "', t = " + p.tempo +" foi memorizada.");
            
            if(perguntas.size() == 1) cond.signalAll();
        } finally {
            lock.unlock();
        }
    }
    
    public void responder(){
        lock.lock();
        try{   
            
            while(perguntas.isEmpty()) {
                try{
                    cond.await();          
                } catch (InterruptedException e) {}
            }         
            
            if(qtdRespondida%2 == 0 && qtdRespondida > 0) {
                System.out.println("Professor foi encher o copo e demorará " + tempoEncher + " milisegundos.");
                try{
                    Thread.sleep(tempoEncher);
                } catch (InterruptedException e) {}
            }
            
            //System.out.println("Professor bebendo agua por " + tempoBeber + " milisegundos.");
            Pergunta p = perguntas.remove(0);
              
            //System.out.println("Professor começou a responder pergunta '" + p.texto + "', t = " + p.tempo);   
            try{
                Thread.sleep(tempoBeber + p.tempo);
            } catch (InterruptedException e) {}                 
            //System.out.println("Pergunta '" + p.texto + "', t = " + p.tempo + " foi respondida.");     
            
            ++qtdRespondida; 
            
            if(perguntas.size() == this.n - 1) cond.signalAll(); 
        } finally {
            lock.unlock();
        }
    }
    
    public static void main(String[] args){
        Scanner in = new Scanner(System.in);
    	
    	int tempoBeber,tempoEncher;
        int n;
        
        System.out.println("Digite o numero de alunos da turma:");
        n = in.nextInt();
        
        System.out.println("Digite o tempo necessário para o professor beber a agua:");
        tempoBeber = in.nextInt();       
        
        System.out.println("Digite o tempo necessário para o professor encher a caneca:");
        tempoEncher = in.nextInt();      
        
        Gerenciador ger = new Gerenciador(n,tempoBeber,tempoEncher, 0);
        Professor prof = new Professor(ger);
        String texto;
        int tempo;
        int numPergs = 0;
        ArrayList<Aluno> alunos = new ArrayList<Aluno>();
        
        for(int i = 0; i < n; ++i){
        	System.out.println("Insira o numero de perguntas do aluno " +(i+1));
            numPergs = in.nextInt();
            ger.totalPergs += numPergs;
            ArrayList<Pergunta> perguntasAluno = new ArrayList<Pergunta>();
            
            for(int j = 0; j < numPergs; ++j){
            	System.out.println("Insira a pergunta:");
                texto = in.next();
                System.out.println("Insira o tempo necessario para responder esta pergunta:");
                tempo = in.nextInt();
                perguntasAluno.add(new Pergunta(texto, tempo));
            }
            
            alunos.add(new Aluno(perguntasAluno, ger));
        }
        
        for(int i = 0; i < n; i++) alunos.get(i).start();
        
        prof.start();
        
        for(int i = 0; i < n; i++) {
            try{
                alunos.get(i).join();
            } catch (InterruptedException e) {}
        }    
        
        try{
            prof.join();
        } catch (InterruptedException e) {}

    }
}

class Aluno extends Thread{
    ArrayList<Pergunta> perguntas = new ArrayList<Pergunta>();
    Gerenciador ger;
    
    public Aluno(ArrayList<Pergunta> perguntas, Gerenciador ger){
        this.perguntas = perguntas;
        this.ger = ger;
    }
    
    public void run(){
        while(!perguntas.isEmpty()){
            ger.perguntar(perguntas.remove(0));
        }
    }
}

class Professor extends Thread {
    Gerenciador ger;
    
    public Professor (Gerenciador ger){
        this.ger = ger;
    }

    public void run() {
        while(ger.totalPergs != ger.qtdRespondida){
            ger.responder();
        }
    }
}

class Pergunta {
    String texto;
    int tempo;
    
    public Pergunta (String texto, int tempo) {
        this.texto = texto;
        this.tempo = tempo;
    }
}