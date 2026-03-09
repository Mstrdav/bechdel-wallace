"""Import de la data Bechdel en préparation de la visualisation.
Ce script charge `data/raw/movies.csv`, vérifie qu'il est bien lisible et affiche quelques statistiques brutes (nombre de films, période couverte, taux de réussite).
"""
import csv
from collections import Counter
from datetime import datetime
from pathlib import Path

DATA_PATH = Path(__file__).resolve().parents[1] / "data" / "raw" / "movies.csv"


def main() -> None:
    if not DATA_PATH.exists():
        raise FileNotFoundError(f"Fichier introuvable : {DATA_PATH}")

    with DATA_PATH.open(newline="", encoding="utf-8") as csvfile:
        reader = csv.DictReader(csvfile)
        rows = list(reader)

    years = sorted({int(row["year"]) for row in rows if row["year"].isdigit()})
    total = len(rows)
    passing = sum(1 for row in rows if row.get("binary") == "PASS")
    failing = sum(1 for row in rows if row.get("binary") == "FAIL")
    budget_missing = sum(1 for row in rows if not row.get("budget"))
    year_counter = Counter(row["year"] for row in rows)

    print("Data Bechdel importée :")
    print(f"  {total:,} films · {passing:,} PASS · {failing:,} FAIL")
    print(f"  Période : {years[0]} → {years[-1]} ({len(years)} années couvertes)")
    print(f"  Budgets manquants : {budget_missing:,}")
    print("  Top 3 années (nombre de films) :")
    for year, count in year_counter.most_common(3):
        print(f"    • {year} : {count} films")

    timestamp = datetime.utcnow().isoformat()
    stamp_file = Path(__file__).resolve().parents[1] / "data" / "raw" / "last_import.txt"
    stamp_file.write_text(f"Dernier import : {timestamp}\n", encoding="utf-8")


if __name__ == "__main__":
    main()
