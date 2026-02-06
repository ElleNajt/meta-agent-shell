#!/Library/Developer/CommandLineTools/usr/bin/python3
"""
ICC (Inter-Claude Communication) Log Visualizer

Visualizes agent communication patterns from meta-agent-shell logs.
"""

import argparse
import json
from collections import defaultdict
from datetime import datetime
from pathlib import Path

import matplotlib.pyplot as plt
import networkx as nx


def load_logs(log_dir: Path, date: str = None) -> list[dict]:
    """Load ICC logs from JSONL files."""
    entries = []

    if date:
        files = [log_dir / f"{date}-icc.jsonl"]
    else:
        files = sorted(log_dir.glob("*-icc.jsonl"))

    for f in files:
        if f.exists():
            with open(f) as fp:
                for line in fp:
                    line = line.strip()
                    if line:
                        entries.append(json.loads(line))

    return entries


def normalize_agent_name(name: str, keep_project: bool = True) -> str:
    """Shorten agent names for display while keeping them identifiable.

    Transforms:
      "Dispatcher Agent @ test-agent-project<2>" -> "Dispatcher@test-agent-project<2>"
      "(test-agent-project)-Worker1 Agent @ test-agent-project" -> "Worker1@test-agent-project"
      "Claude Code Agent @ meta-agent-shell<2>" -> "Claude Code@meta-agent-shell<2>"
    """
    # Handle "(project)-Name Agent @ project" format
    if name.startswith("(") and ")-" in name:
        # Extract just the agent name part after ")-"
        name = name.split(")-", 1)[1]

    # Handle "Name Agent @ project" format
    if " Agent @ " in name:
        parts = name.split(" Agent @ ")
        agent_name = parts[0]
        project = parts[1] if len(parts) > 1 else ""
        if keep_project and project:
            return f"{agent_name}@{project}"
        return agent_name

    return name


def extract_project(name: str) -> str:
    """Extract project name from agent identifier."""
    if " @ " in name:
        return name.split(" @ ")[-1]
    if name.startswith("(") and ")-" in name:
        return name.split(")-")[0][1:]
    return "unknown"


def build_graph(entries: list[dict], raw: bool = False) -> nx.DiGraph:
    """Build a directed graph of agent communications."""
    G = nx.DiGraph()
    edge_data = defaultdict(lambda: {"count": 0, "types": defaultdict(int)})

    for entry in entries:
        if raw:
            src = entry["from"]
            dst = entry["to"]
        else:
            src = normalize_agent_name(entry["from"])
            dst = normalize_agent_name(entry["to"])
        edge_data[(src, dst)]["count"] += 1
        edge_data[(src, dst)]["types"][entry["type"]] += 1

    for (src, dst), data in edge_data.items():
        # Format label as "type(count), type(count), ..."
        type_labels = []
        for t, count in sorted(data["types"].items()):
            if count > 1:
                type_labels.append(f"{t}({count})")
            else:
                type_labels.append(t)
        G.add_edge(src, dst, weight=data["count"], label=", ".join(type_labels))

    return G


def plot_communication_graph(
    G: nx.DiGraph, output: Path = None, title: str = "ICC Communication Graph"
):
    """Plot the agent communication graph."""
    if len(G.nodes()) == 0:
        print("No communications to visualize")
        return

    fig, ax = plt.subplots(figsize=(12, 8))

    # Layout
    pos = nx.spring_layout(G, k=2, iterations=50, seed=42)

    # Node sizes based on degree
    degrees = dict(G.degree())
    node_sizes = [300 + degrees[n] * 100 for n in G.nodes()]

    # Edge widths based on message count
    weights = [G[u][v]["weight"] for u, v in G.edges()]
    max_weight = max(weights) if weights else 1
    edge_widths = [1 + (w / max_weight) * 4 for w in weights]

    # Draw
    nx.draw_networkx_nodes(
        G,
        pos,
        node_size=node_sizes,
        node_color="lightblue",
        edgecolors="black",
        linewidths=1,
        ax=ax,
    )
    nx.draw_networkx_labels(G, pos, font_size=8, ax=ax)
    nx.draw_networkx_edges(
        G,
        pos,
        width=edge_widths,
        edge_color="gray",
        arrows=True,
        arrowsize=15,
        connectionstyle="arc3,rad=0.1",
        ax=ax,
    )

    # Edge labels (action types)
    edge_labels = {(u, v): G[u][v]["label"] for u, v in G.edges()}
    nx.draw_networkx_edge_labels(G, pos, edge_labels, font_size=7, ax=ax)

    ax.set_title(title)
    ax.axis("off")
    plt.tight_layout()

    if output:
        plt.savefig(output, dpi=150, bbox_inches="tight")
        print(f"Saved to {output}")
    else:
        plt.show()


def plot_timeline(entries: list[dict], output: Path = None):
    """Plot message timeline showing activity over time."""
    if not entries:
        print("No entries to visualize")
        return

    # Parse timestamps
    times = []
    for e in entries:
        t = datetime.fromisoformat(e["timestamp"])
        times.append(t)

    if not times:
        print("No valid timestamps found")
        return

    fig, ax = plt.subplots(figsize=(12, 4))

    # Bin by minute
    minute_counts = defaultdict(int)
    for t in times:
        key = t.replace(second=0, microsecond=0)
        minute_counts[key] += 1

    if minute_counts:
        sorted_times = sorted(minute_counts.keys())
        counts = [minute_counts[t] for t in sorted_times]

        ax.bar(sorted_times, counts, width=0.0005, color="steelblue")
        ax.set_xlabel("Time")
        ax.set_ylabel("Messages")
        ax.set_title("ICC Message Activity Over Time")
        plt.xticks(rotation=45)

    plt.tight_layout()

    if output:
        plt.savefig(output, dpi=150, bbox_inches="tight")
        print(f"Saved to {output}")
    else:
        plt.show()


def print_stats(entries: list[dict], raw: bool = False):
    """Print summary statistics."""
    if not entries:
        print("No entries found")
        return

    def name(n):
        return n if raw else normalize_agent_name(n)

    print(f"\n=== ICC Log Statistics ===")
    print(f"Total messages: {len(entries)}")

    # Type breakdown
    types = defaultdict(int)
    for e in entries:
        types[e["type"]] += 1
    print(f"\nBy type:")
    for t, count in sorted(types.items()):
        print(f"  {t}: {count}")

    # Most active senders
    senders = defaultdict(int)
    for e in entries:
        senders[name(e["from"])] += 1
    print(f"\nTop senders:")
    for sender, count in sorted(senders.items(), key=lambda x: -x[1])[:10]:
        print(f"  {sender}: {count}")

    # Most messaged agents
    receivers = defaultdict(int)
    for e in entries:
        receivers[name(e["to"])] += 1
    print(f"\nTop receivers:")
    for receiver, count in sorted(receivers.items(), key=lambda x: -x[1])[:10]:
        print(f"  {receiver}: {count}")

    # Projects
    projects = set()
    for e in entries:
        projects.add(extract_project(e["to"]))
    print(f"\nProjects: {', '.join(sorted(projects))}")


def main():
    parser = argparse.ArgumentParser(description="Visualize ICC logs")
    parser.add_argument(
        "--log-dir",
        default=Path.home() / ".meta-agent-shell/logs",
        type=Path,
        help="Log directory",
    )
    parser.add_argument("--date", help="Specific date (YYYY-MM-DD)")
    parser.add_argument("--output", "-o", type=Path, help="Output file (PNG)")
    parser.add_argument(
        "--timeline", action="store_true", help="Show timeline instead of graph"
    )
    parser.add_argument("--stats", action="store_true", help="Print statistics only")
    parser.add_argument(
        "--raw", action="store_true", help="Use raw names without normalization"
    )
    args = parser.parse_args()

    entries = load_logs(args.log_dir, args.date)

    if args.stats:
        print_stats(entries, raw=args.raw)
        return

    if args.timeline:
        plot_timeline(entries, args.output)
    else:
        G = build_graph(entries, raw=args.raw)
        title = f"ICC Communications"
        if args.date:
            title += f" ({args.date})"
        plot_communication_graph(G, args.output, title)


if __name__ == "__main__":
    main()
