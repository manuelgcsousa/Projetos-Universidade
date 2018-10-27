namespace CleanicaFinal
{
    using System;
    using System.Data.Entity;
    using System.ComponentModel.DataAnnotations.Schema;
    using System.Linq;

    public partial class CleanicaContext : DbContext
    {
        public CleanicaContext()
            : base("name=CleanicaContext")
        {
        }

        public virtual DbSet<Cliente> Clientes { get; set; }
        public virtual DbSet<Formulario_de_Realizacao> Formulario_de_Realizacao { get; set; }
        public virtual DbSet<Formulario_de_Satisfacao> Formulario_de_Satisfacao { get; set; }
        public virtual DbSet<Funcionario> Funcionarios { get; set; }
        public virtual DbSet<Servico> Servicoes { get; set; }
        public virtual DbSet<ServicosRealizado> ServicosRealizados { get; set; }
        public virtual DbSet<Utilizador> Utilizadors { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            modelBuilder.Entity<Cliente>()
                .Property(e => e.Utilizador_email)
                .IsUnicode(false);

            modelBuilder.Entity<Cliente>()
                .Property(e => e.morada)
                .IsUnicode(false);

            modelBuilder.Entity<Cliente>()
                .HasMany(e => e.Formulario_de_Satisfacao)
                .WithRequired(e => e.Cliente)
                .HasForeignKey(e => e.Cliente_Utilizador_email)
                .WillCascadeOnDelete(false);

            modelBuilder.Entity<Cliente>()
                .HasMany(e => e.Formulario_de_Realizacao)
                .WithRequired(e => e.Cliente)
                .HasForeignKey(e => e.Cliente_Utilizador_email)
                .WillCascadeOnDelete(false);

            modelBuilder.Entity<Cliente>()
                .HasMany(e => e.ServicosRealizados)
                .WithRequired(e => e.Cliente)
                .HasForeignKey(e => e.Cliente_Utilizador_email)
                .WillCascadeOnDelete(false);

            modelBuilder.Entity<Formulario_de_Realizacao>()
                .Property(e => e.Cliente_Utilizador_email)
                .IsUnicode(false);

            modelBuilder.Entity<Formulario_de_Realizacao>()
                .Property(e => e.FuncionarioUtilizador_email)
                .IsUnicode(false);

            modelBuilder.Entity<Formulario_de_Realizacao>()
                .Property(e => e.observacoes)
                .IsUnicode(false);

            modelBuilder.Entity<Formulario_de_Satisfacao>()
                .Property(e => e.Cliente_Utilizador_email)
                .IsUnicode(false);

            modelBuilder.Entity<Formulario_de_Satisfacao>()
                .Property(e => e.Funcionario_Utilizador_email)
                .IsUnicode(false);

            modelBuilder.Entity<Formulario_de_Satisfacao>()
                .Property(e => e.sugestoes)
                .IsUnicode(false);

            modelBuilder.Entity<Formulario_de_Satisfacao>()
                .Property(e => e.pendente)
                .IsUnicode(false);

            modelBuilder.Entity<Funcionario>()
                .Property(e => e.Utilizador_email)
                .IsUnicode(false);

            modelBuilder.Entity<Funcionario>()
                .Property(e => e.IBAN)
                .IsUnicode(false);

            modelBuilder.Entity<Funcionario>()
                .Property(e => e.morada)
                .IsUnicode(false);

            modelBuilder.Entity<Funcionario>()
                .HasMany(e => e.Formulario_de_Realizacao)
                .WithRequired(e => e.Funcionario)
                .WillCascadeOnDelete(false);

            modelBuilder.Entity<Funcionario>()
                .HasMany(e => e.Formulario_de_Satisfacao)
                .WithRequired(e => e.Funcionario)
                .HasForeignKey(e => e.Funcionario_Utilizador_email)
                .WillCascadeOnDelete(false);

            modelBuilder.Entity<Funcionario>()
                .HasMany(e => e.ServicosRealizados)
                .WithMany(e => e.Funcionarios)
                .Map(m => m.ToTable("Funcionario_ServicosRealizados").MapRightKey("ServicosRealizados_id_ServicosRealizados"));

            modelBuilder.Entity<Servico>()
                .Property(e => e.nome)
                .IsUnicode(false);

            modelBuilder.Entity<Servico>()
                .Property(e => e.descricao)
                .IsUnicode(false);

            modelBuilder.Entity<Servico>()
                .HasMany(e => e.Funcionarios)
                .WithRequired(e => e.Servico)
                .HasForeignKey(e => e.Servico_id_servico)
                .WillCascadeOnDelete(false);

            modelBuilder.Entity<ServicosRealizado>()
                .Property(e => e.Cliente_Utilizador_email)
                .IsUnicode(false);

            modelBuilder.Entity<ServicosRealizado>()
                .Property(e => e.morada)
                .IsUnicode(false);

            modelBuilder.Entity<ServicosRealizado>()
                .Property(e => e.dimensao)
                .IsUnicode(false);

            modelBuilder.Entity<ServicosRealizado>()
                .Property(e => e.prods)
                .IsUnicode(false);

            modelBuilder.Entity<ServicosRealizado>()
                .Property(e => e.aspectosimp)
                .IsUnicode(false);

            modelBuilder.Entity<ServicosRealizado>()
                .Property(e => e.pendente)
                .IsUnicode(false);

            modelBuilder.Entity<Utilizador>()
                .Property(e => e.email)
                .IsUnicode(false);

            modelBuilder.Entity<Utilizador>()
                .Property(e => e.username)
                .IsUnicode(false);

            modelBuilder.Entity<Utilizador>()
                .Property(e => e.password)
                .IsUnicode(false);

            modelBuilder.Entity<Utilizador>()
                .Property(e => e.estatuto)
                .IsUnicode(false);

            modelBuilder.Entity<Utilizador>()
                .HasOptional(e => e.Cliente)
                .WithRequired(e => e.Utilizador);

            modelBuilder.Entity<Utilizador>()
                .HasOptional(e => e.Funcionario)
                .WithRequired(e => e.Utilizador);
        }
    }
}
