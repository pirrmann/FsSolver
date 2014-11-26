using System;
using System.Linq;
using System.Collections.Generic;

using FsSolver;
using FsSolver.Rules;

using NUnit.Framework;

namespace ExplicitBindersTests
{
    public class Rfq
    {
        public long RfqId { get; set; }
        public decimal? TotalExecFees { get; set; }
        public decimal? FeesPerLot { get; set; }
        public IEnumerable<Underlying> Underlyings { get; set; }
    }

    public class Underlying
    {
        public string UnderlyingId { get; set; }
        public decimal? ExecFees { get; set; }
        public IEnumerable<Leg> Legs { get; set; }
    }

    public class Leg
    {
        public long LegId { get; set; }
        public decimal Size { get; set; }
    }

    public static class BinderBuilder
    {
        public static IEnumerable<Binder> GetBinders(Rfq rfq)
        {
            yield return Binder.Scoped(() => rfq.TotalExecFees, new[] { "TotalExecFees", rfq.RfqId.ToString() });
            yield return Binder.Scoped(() => rfq.FeesPerLot, new[] { "FeesPerLot", rfq.RfqId.ToString() });
            foreach(var u in rfq.Underlyings)
            {
                yield return Binder.Scoped(() => u.ExecFees, new[] { "ExecFees", u.UnderlyingId, rfq.RfqId.ToString() });
                foreach(var l in u.Legs)
                {
                    yield return Binder.Scoped(() => l.Size, new[] { "Size", l.LegId.ToString(), u.UnderlyingId, rfq.RfqId.ToString() });
                }
            }
        }

        public static Scope GetScope(Rfq rfq)
        {
            return Scope.Create(
                rfq.RfqId.ToString(),
                rfq.Underlyings.Select(u =>
                    Scope.Create(
                        u.UnderlyingId,
                        u.Legs.Select(l =>
                            Scope.Create(
                                l.LegId.ToString(),
                                new Scope[] { }
                        )).ToArray())
                    ).ToArray());
        }
    }

    public class UsingTheSolverFromCSharpWithExplicitBindersTests
    {
        [Test]
        public void Using_the_solver_from_C_Sharp_with_explicit_binders()
        {
            var rfq = new Rfq
            {
                RfqId = 1,
                FeesPerLot = 1.0M,
                Underlyings = new [] {
                    new Underlying {
                        UnderlyingId = "u",
                        Legs = new [] {
                            new Leg { LegId = 1, Size = 100M },
                            new Leg { LegId = 2, Size = 100M }
                        }
                    }
                }
            };

            var scope = BinderBuilder.GetScope(rfq);
            var binders = BinderBuilder.GetBinders(rfq);

            var settings = new CSharpTestsRules.Settings(true);
            var rules = CSharpTestsRules.GetRules(settings);

            var problem = BoundProblem.Create(rules, scope, binders);
            problem.Solve();

            Assert.AreEqual(200M, rfq.TotalExecFees);
            Assert.AreEqual(200M, rfq.Underlyings.First().ExecFees);
        }
    }
}
