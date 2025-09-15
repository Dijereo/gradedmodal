import React from "react";

export default function TabButtons({
    states,
    onClicks,
}) {
    const labels = ["Instructions", "Model", "Tableau"];
    return (
        <div style={{ display: "flex", gap: "8px" }}>
            {states.map((state, i) => {
                if (state === "hidden") return null;

                return (
                    <button
                        key={i}
                        onClick={onClicks[i]}
                        style={{
                            padding: "8px 16px",
                            cursor: "pointer",
                            backgroundColor: state === "active" ? 'var(--neutral-color)' : 'var(--inactive-color)',
                            color: "var(--text-color)",
                            border: state === "active" ? "1px solid var(--border-color)" : "1px solid #999",
                            borderRadius: "4px",
                        }}
                    >
                        {labels[i]}
                    </button>
                );
            })}
        </div>
    );
}
