using System;
using System.Linq;
using System.Collections.Generic;

using FsSolver;
using FsSolver.Rules;

using NUnit.Framework;

namespace ReflectedBindersTests
{
    public class Rfq
    {
        public decimal? TotalExecFees { get; set; }
        public decimal? FeesPerLot { get; set; }
        public decimal? SomePropertyWithoutSetter { get { return null; } }
        public IEnumerable<Underlying> Underlyings { get; set; }
    }

    public class Underlying
    {
        public decimal? ExecFees { get; set; }
        public decimal? WeightedDelta { get; set; }
        public decimal? BaseSize { get; set; }
        public IEnumerable<Leg> Legs { get; set; }
    }

    public class Leg
    {
        public SolverValue Size { get; set; }
        public decimal? Delta { get; set; }
    }

    public class UsingTheSolverFromCSharpWithReflectedBindersTests
    {
        [Test]
        public void Using_the_solver_from_C_Sharp_with_reflected_binders()
        {
            var rfq = new Rfq
            {
                FeesPerLot = 1.0M,
                Underlyings = new[] {
                    new Underlying {
                        BaseSize = 100M,
                        Legs = new [] {
                            new Leg { Size = 100M, Delta = 0.4M },
                            new Leg { Size = 100M, Delta = -0.3M }
                        }
                    }
                }
            };

            var settings = new CSharpTestsRules.Settings(true);
            var rules = CSharpTestsRules.GetRules(settings);

            var problem = BoundProblem.Create(rules, rfq);
            var incoherencies = problem.Solve();

            Assert.IsEmpty(incoherencies);

            Assert.IsNull(rfq.SomePropertyWithoutSetter);
            Assert.AreEqual(200M, rfq.TotalExecFees);

            var underlying = rfq.Underlyings.First();
            Assert.AreEqual(200M, underlying.ExecFees);
            Assert.AreEqual(0.1M, underlying.WeightedDelta);
        }

        [Test]
        public void Error_reporting_from_C_Sharp_with_reflected_binders()
        {
            var rfq = new Rfq
            {
                FeesPerLot = 1.0M,
                Underlyings = new[] {
                    new Underlying {
                        BaseSize = 100M,
                        WeightedDelta = 0.11M,
                        Legs = new [] {
                            new Leg { Size = SolverValue.NewProvidedNoConflict(100M), Delta = 0.4M },
                            new Leg { Size = SolverValue.NewProvidedNoConflict(100M), Delta = -0.3M }
                        }
                    }
                }
            };

            var settings = new CSharpTestsRules.Settings(true);
            var rules = CSharpTestsRules.GetRules(settings);

            var problem = BoundProblem.Create(rules, rfq);
            var incoherencies = problem.Solve();

            Assert.AreEqual(4, incoherencies.Length);
        }
    }
}
