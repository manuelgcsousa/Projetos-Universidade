namespace CleanicaFinal.Models
{
    using System;
    using System.Data.Entity;
    using System.ComponentModel.DataAnnotations.Schema;
    using System.Linq;

    public partial class FuncServContext : DbContext
    {
        public FuncServContext()
            : base("name=FuncServContext")
        {
        }

        public virtual DbSet<Funcionario_ServicosRealizados> Funcionario_ServicosRealizados { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            modelBuilder.Entity<Funcionario_ServicosRealizados>()
                .Property(e => e.Funcionario_Utilizador_email)
                .IsUnicode(false);
        }
    }
}
