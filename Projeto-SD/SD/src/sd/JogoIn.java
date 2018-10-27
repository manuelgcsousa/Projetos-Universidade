/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package sd;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.HashMap;

/**
 *
 * @author carlos
 */
public class JogoIn implements Runnable {
    private BufferedReader in;
    private Jogo j;
    private String player; //NOme do jogador a quem a thread esta destinada.
    private int playerNo; //Numero do jogador a quem a thread esta destinada
    
    public JogoIn(BufferedReader in, Jogo j, String player, int playerNo) {
        this.in = in;
        this.j = j;
        this.player = player;
        this.playerNo = playerNo;
    }
    
    public synchronized void removeHero1(String player) {
        Integer i = this.j.getTeam1().get(player);
        i = null;
    }
    
    public synchronized void removeHero2(String player) {
        Integer i = this.j.getTeam2().get(player);
        i = null;
    }
    
    @Override
    public void run() {
        try {
            String msg;
            
            while ( !((msg = this.in.readLine()).equals("s")) ) {
                if ("c".equals(msg)) {
                    if (this.playerNo >= 0 && this.playerNo <= 4){
                        removeHero1(msg);
                    }
                    else if(this.playerNo >= 5 && this.playerNo <= 9){
                        removeHero2(msg);
                    }
                } else if(isValid(msg)) {
                    if (this.playerNo >= 0 && this.playerNo <= 4){
                        addHeroT1(msg);
                    }
                    else if(this.playerNo >= 5 && this.playerNo <= 9){
                        addHeroT2(msg);
                    }  
                } else {
                    String s;
                    BufferedWriter bW = this.j.getBuffs().get(this.player);
                    if (Integer.parseInt(msg) > 30){
                        s = "Herói não existente.\nIntroduza outro valor!";
                    } else {
                        s = "Herói já escolhido, escolha outro por favor!";
                    }
                    bW.write(s);
                    bW.newLine();
                    bW.flush();
                }
            }
        } catch (IOException | NullPointerException ex) {
            System.out.println("A ligaçao com esse cliente ja foi fechada.");
        }
    }

    private boolean isValid(String msg) {
        if(this.playerNo >= 0 && this.playerNo <= 4 ){
            HashMap<Integer,String> hm = new HashMap<>(this.j.getHeroes());
            hm.keySet().removeAll(this.j.getTeam1().values());
            String hero = hm.get(Integer.parseInt(msg));
            if (hero == null) return false;
        }
        
        if(this.playerNo >= 5 && this.playerNo <= 9 ){
            HashMap<Integer,String> hm = new HashMap<>(this.j.getHeroes());
            hm.keySet().removeAll(this.j.getTeam2().values());
            String hero = hm.get(Integer.parseInt(msg));
            if (hero == null) return false;
        }
        return true;
    }
    
    public synchronized void multicast1(){
        Thread[] buffs = new Thread[5];
        int p = 0;
        String s = "";
        for(String st : this.j.getTeam1().keySet()){
            s = s + " " + st + " -> " + this.j.getTeam1().get(st) + "\n";
        }
        s += "Escolha um dos Seguintes Heróis\n";
                
        HashMap<Integer,String> hm = new HashMap<>(this.j.getHeroes());
        hm.keySet().removeAll(this.j.getTeam1().values());
        
        for(Integer i : hm.keySet()){
             s = s + i + " -> " + this.j.getHeroes().get(i) + "\n";
        }
        for(String st : this.j.getTeam1().keySet()){
            buffs[p] = new Thread(new TalkToPlayers(this.j.getBuffs().get(st),s));
            buffs[p].start();
            p++;
        }
    }    
    public synchronized void multicast2(){
        Thread[] buffs = new Thread[5];
        int p = 0;
        String s = "";
        for (String st : this.j.getTeam2().keySet()) {
            s = s + " " + st + " -> " + this.j.getTeam2().get(st) + "\n";
        }
        s += "Escolha um dos Seguintes Heróis\n";
                
        HashMap<Integer,String> hm = new HashMap<>(this.j.getHeroes());
        hm.keySet().removeAll(this.j.getTeam2().values());
        
        for(Integer i : hm.keySet()){
             s = s + i + " -> " + this.j.getHeroes().get(i) + "\n";
        }
        
        for (String st : this.j.getTeam2().keySet()) {
            buffs[p] = new Thread(new TalkToPlayers(this.j.getBuffs().get(st),s));
            buffs[p].start();
            p++;
        }
        
    } 

    public void addHeroT1(String msg) {
        if (msg != null) {
            addHero1(Integer.parseInt(msg),this.j.getPlayers().get(this.playerNo));
            multicast1();
        }
    }

    public void addHeroT2(String msg) {
        if (msg != null) {
            addHero2(Integer.parseInt(msg),this.j.getPlayers().get(this.playerNo));
            multicast2();
        }
    }
    
    public synchronized void addHero1(int hero, String user) {
        this.j.getTeam1().put(user, hero);
    }
    
    public synchronized void addHero2(int hero, String user) {
        this.j.getTeam2().put(user, hero);
    }
}
