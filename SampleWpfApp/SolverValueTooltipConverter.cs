using System;
using FsSolver;
using System.Globalization;
using System.Windows.Data;

namespace SampleWpfApp
{
    class SolverValueTooltipConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            var solverValue = value as SolverValue;
            if (solverValue == null)
            {
                return null;
            }

            return solverValue.Description;
        }


        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotSupportedException();
        }
    }
}
