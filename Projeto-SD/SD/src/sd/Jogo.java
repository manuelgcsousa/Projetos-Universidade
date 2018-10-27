package sd;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

/**
 *
 * @author carlo
 */
public class Jogo implements Runnable {
    
    private HashMap<String,Utilizador> users;
    private HashMap<String,Integer> team1;  //O HashMap contém o nome do Jogador e o ID do Herói escolhido por cada um da 1ª Equipa.
    private HashMap<String,Integer> team2;  //O HashMap contém o nome do Jogador e o ID do Herói escolhido por cada um da 2ª Equipa.
    private HashMap<String,BufferedReader> buffsIn; //Lista de Inputs de cada um dos 10 jogadores envolvidos no jogo
    private HashMap<String,BufferedWriter> buffs;   //Lista de Output de cada um dos 10 jogadores envolvidos no jogo
    private HashMap<Integer,String> heroes; // Lista de heróis que podem ser escolhidos pelos jogadores;
    private ArrayList<String> players;  // Lista ordenada com os primeiros 5 elementos a ser a 1ª equipa e os segundos elementos a 2ª equipa
    
    public Jogo(ArrayList<Utilizador> users, HashMap<String,BufferedWriter> buffs, HashMap<String,BufferedReader> buffsIn, int rank){        
        this.users = new HashMap<>();
        ArrayList<Utilizador> ordered = new ArrayList<>(10);
        
        for(Utilizador u : users){
            this.users.put(u.getUsername(), u);
            if(u.getRank() == rank){
                ordered.add(u);
            }
        }
        
        for(Utilizador u : users){
            this.users.put(u.getUsername(), u);
            if(u.getRank() == rank + 1){
                ordered.add(u);
            }
        }
        
        this.players = new ArrayList<>(10);
        this.players.add(ordered.get(0).getUsername());
        this.players.add(ordered.get(1).getUsername());
        this.players.add(ordered.get(2).getUsername());
        this.players.add(ordered.get(8).getUsername());
        this.players.add(ordered.get(9).getUsername());
        this.players.add(ordered.get(3).getUsername());
        this.players.add(ordered.get(4).getUsername());
        this.players.add(ordered.get(5).getUsername());
        this.players.add(ordered.get(6).getUsername());
        this.players.add(ordered.get(7).getUsername());
        
        //As equipas são a mediana dos ranks dos jogadores
        this.team1 = new HashMap<>();
        this.team1.put(ordered.get(0).getUsername(),null);
        this.team1.put(ordered.get(1).getUsername(),null);
        this.team1.put(ordered.get(2).getUsername(),null);
        this.team1.put(ordered.get(8).getUsername(),null);
        this.team1.put(ordered.get(9).getUsername(),null);
        
        this.team2 = new HashMap<>();
        this.team2.put(ordered.get(3).getUsername(),null);
        this.team2.put(ordered.get(4).getUsername(),null);
        this.team2.put(ordered.get(5).getUsername(),null);
        this.team2.put(ordered.get(6).getUsername(),null);
        this.team2.put(ordered.get(7).getUsername(),null);
        
        this.buffs = buffs;
        this.buffsIn = buffsIn;
        
        this.heroes = new HashMap<>(30);
        this.heroes.put(1,"Doomfist");
        this.heroes.put(2,"Genji");
        this.heroes.put(3,"McCree");
        this.heroes.put(4,"Pharah");
        this.heroes.put(5,"Reaper");
        this.heroes.put(6,"Soldier: 76");
        this.heroes.put(7,"Sombra");
        this.heroes.put(8,"Tracer");
        this.heroes.put(9,"Bastion");
        this.heroes.put(10,"Hanzo");
        this.heroes.put(11,"Junkrat");
        this.heroes.put(12,"Mei");
        this.heroes.put(13,"Torbjorn");
        this.heroes.put(14,"Widowmaker");
        this.heroes.put(15,"D.Va");
        this.heroes.put(16,"Orisa");
        this.heroes.put(17,"Reinhardt");
        this.heroes.put(18,"Roadhog");
        this.heroes.put(19,"Winston");
        this.heroes.put(20,"Zarya");
        this.heroes.put(21,"Ana");
        this.heroes.put(22,"Lúcio");
        this.heroes.put(23,"Mercy");
        this.heroes.put(24,"Moira");
        this.heroes.put(25,"Symmetra");
        this.heroes.put(26,"Carlos Pedrosa");
        this.heroes.put(27,"Manuel Sousa");
        this.heroes.put(28,"David Sousa");
        this.heroes.put(29,"The Destroyer");
        this.heroes.put(30,"Iron Man");
    }
    
    public synchronized HashMap<String, Integer> getTeam1() {
        return this.team1;
    }

    public synchronized HashMap<String, Integer> getTeam2() {
        return this.team2;
    }

    public synchronized HashMap<String, BufferedReader> getBuffsIn() {
        return this.buffsIn;
    }

    public synchronized HashMap<String, BufferedWriter> getBuffs() {
        return this.buffs;
    }

    public synchronized HashMap<Integer, String> getHeroes() {
        return this.heroes;
    }

    public synchronized ArrayList<String> getPlayers() {
        return this.players;
    }    

    public synchronized boolean todosTemHero() {
        for(String s : this.team1.keySet()){
            if(this.team1.get(s) == null) return false;
        }
        
        for(String s : this.team2.keySet()){
            if(this.team2.get(s) == null) return false;
        }
        
        return true;
    }

    public synchronized void addRank(HashMap<String, Integer> team) {
        for(String s : team.keySet()){
            int rank = this.users.get(s).getRank();
            if(rank < 9){
                rank++;
            }
        }
    }

    public synchronized void subRank(HashMap<String, Integer> team) {
        for(String s : team.keySet()){
            int rank = this.users.get(s).getRank();
            if(rank > 0){
                rank--;
            }
        }
    }

    @Override
    public void run() {
        long startTime = System.currentTimeMillis();
        long endTime = 1000000;
        int p = 0;
        Thread[] buffs = new Thread[10];

        String s = "Selecione um Herói\n";
        
        for(Integer i : this.heroes.keySet()) {
            s = s + i + " -> " + this.heroes.get(i) + "\n";
        }
        
        for(String st : this.buffs.keySet()) {
            buffs[p] = new Thread(new TalkToPlayers(this.buffs.get(st),s));
            buffs[p].start();
            p++;
        }
                
        Thread[] threads = new Thread[10];
        int t = 0;
        for(t = 0; t < 10; t++){
            BufferedReader bR = this.buffsIn.get(this.players.get(t));
            threads[t] = new Thread(new JogoIn(bR,this,this.players.get(t),t));
            threads[t].start();
        }
                
        try {
            while ( endTime - startTime <= 30000 ){
                endTime = System.currentTimeMillis();     
            } 
            
            boolean todosJogam = todosTemHero();
            
            if(todosJogam){
                Random r = new Random();
                int low = 0;
                int high = 2;
                int result = r.nextInt(high - low) + low;
                if(result == 0){
                    //Team 1 Ganhou
                    addRank(this.team1);
                    subRank(this.team2);
                    
                    p = 0;
                    for(String st : this.team1.keySet()) {
                        buffs[p] = new Thread(new TalkToPlayers(this.buffs.get(st),"You Won, Congratz!\n"));
                        buffs[p].start();
                        p++;
                    }
                    
                    for(String st : this.team2.keySet()) {
                        buffs[p] = new Thread(new TalkToPlayers(this.buffs.get(st),"You Lose!\n"));
                        buffs[p].start();
                        p++;
                    }
                    
                } else if (result == 1){
                    //Team 2 Ganhou
                    addRank(this.team2);
                    subRank(this.team1);
                    
                    p = 0;
                    for(String st : this.team2.keySet()) {
                        buffs[p] = new Thread(new TalkToPlayers(this.buffs.get(st),"You Won, Congratz!\n"));
                        buffs[p].start();
                        p++;
                    }
                    
                    for (String st : this.team1.keySet()) {
                        buffs[p] = new Thread(new TalkToPlayers(this.buffs.get(st),"You Lose!\n"));
                        buffs[p].start();
                        p++;
                    }
                }
            } else {
                p = 0;
                for (String st : this.buffs.keySet()) {
                    buffs[p] = new Thread(new TalkToPlayers(this.buffs.get(st),"O Jogo terminou porque os herois nao foram escolhidos a tempo\n"));
                    buffs[p].start();
                    p++;
                }
            }
            
            p = 0;
            for (String st : this.buffs.keySet()) {
                buffs[p] = new Thread(new TalkToPlayers(this.buffs.get(st),"Digite 'quit' para sair do Jogo.\n-> "));
                buffs[p].start();
                p++;
            } 
        } catch(Exception e){
            e.printStackTrace();
        }
    }
}