using System;
using FsSolver;
using System.Globalization;
using System.Windows.Data;

namespace SampleWpfApp
{
    class SolverValueConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            var solverValue = value as SolverValue;
            if (solverValue == null)
            {
                return null;
            }

            var nullableValue = solverValue.AsNullable;

            return nullableValue.HasValue ? nullableValue.Value.ToString(culture) : null;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            var stringValue = value as string;
            if (stringValue == null)
            {
                return null;
            }

            decimal d;
            if(decimal.TryParse(stringValue, NumberStyles.Any, culture, out d))
            {
                return (SolverValue)d;
            }

            return null;
        }
    }
}
