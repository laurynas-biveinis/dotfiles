"""Common code for the various *2qif scripts."""


class QIFInvestmentTransaction:
    """An investment transaction in QIF format."""

    def __init__(self, *, date, security, fee, amount, unit_price, count):
        """Create a QIF investment transaction."""
        self._date = date
        self._security = security
        self._fee = fee
        self._amount = amount
        self._unit_price = unit_price
        self._count = count

    def __str__(self):
        """Return a string representation of this transaction."""
        return (
            f"QIFInvestmentTransaction(Date: {self._date}, "
            f"amount: {self._amount}, unit price: {self._unit_price}, "
            f"count: {self._count}, fee: {self._fee})"
        )

    def write(self, f):
        """Serialize this transaction to QIF."""
        f.write(f"D{self._date}\n")
        f.write(f"Y{self._security}\n")
        f.write("NBuy\n")
        f.write(f"O{self._fee}\n")
        f.write(f"T{self._amount}\n")
        f.write(f"I{self._unit_price}\n")
        f.write(f"Q{self._count}\n")
        f.write("^\n")
