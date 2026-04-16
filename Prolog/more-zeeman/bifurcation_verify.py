"""
Zeeman catastrophe machine bifurcation diagram generator.

Vectorized numpy implementation — runs in seconds, not minutes.
Sweeps the control point and finds stable equilibria via gradient descent.

Usage:
    python3 bifurcation_verify.py
"""

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt


# Match the JS simulation parameters
CANVAS = 600
WHEEL_CX, WHEEL_CY = CANVAS / 2, CANVAS / 2
FIXED_X, FIXED_Y = CANVAS / 2, CANVAS * 0.1
ATTACH_R = CANVAS * 0.083
SPRING_K = 2.0
NAT_LEN = 100


def gradient_vec(angles, cx, cy):
    """Vectorized gradient computation for an array of angles at one control point."""
    R = ATTACH_R
    Px = R * np.cos(angles)
    Py = R * np.sin(angles)

    Fx = FIXED_X - WHEEL_CX
    Fy = FIXED_Y - WHEEL_CY
    Cx = cx - WHEEL_CX
    Cy = cy - WHEEL_CY

    L1 = np.hypot(Px - Fx, Py - Fy)
    L2 = np.hypot(Px - Cx, Py - Cy)

    T1 = np.where((L1 > NAT_LEN) & (L1 > 1e-6),
                  SPRING_K * (1 - NAT_LEN / L1) * (Fx * np.sin(angles) - Fy * np.cos(angles)),
                  0.0)
    T2 = np.where((L2 > NAT_LEN) & (L2 > 1e-6),
                  SPRING_K * (1 - NAT_LEN / L2) * (Cx * np.sin(angles) - Cy * np.cos(angles)),
                  0.0)
    return R * (T1 + T2)


def find_equilibria_fast(cx, cy, n_starts=36):
    """Find stable equilibria via vectorized gradient descent."""
    angles = np.linspace(0, 2 * np.pi, n_starts, endpoint=False)

    for _ in range(400):
        g = gradient_vec(angles, cx, cy)
        angles -= np.clip(0.001 * g, -0.1, 0.1)
        angles = angles % (2 * np.pi)

    # Check stability
    d = 0.01
    g_plus = gradient_vec(angles + d, cx, cy)
    g_minus = gradient_vec(angles - d, cx, cy)
    stab = (g_plus - g_minus) / (2 * d)

    # Keep only stable equilibria
    stable = angles[stab > 0]
    stable = stable % (2 * np.pi)

    # Deduplicate
    if len(stable) == 0:
        return np.array([])
    stable.sort()
    diffs = np.diff(stable)
    keep = np.concatenate([[True], diffs > 0.15])
    return stable[keep]


def main():
    N = 120  # grid resolution
    print(f'Computing {N}x{N} = {N*N} control positions...')

    cx_range = np.linspace(CANVAS * 0.1, CANVAS * 0.9, N)
    cy_range = np.linspace(CANVAS * 0.1, CANVAS * 0.9, N)

    all_cx, all_cy, all_angles, all_letters = [], [], [], []
    all_dists, all_snap_angles = [], []

    for i, cx in enumerate(cx_range):
        if i % 20 == 0:
            print(f'  row {i}/{N}')
        for cy in cy_range:
            eqs = find_equilibria_fast(cx, cy, n_starts=36)
            dist = np.hypot(cx - WHEEL_CX, cy - WHEEL_CY)
            for eq in eqs:
                all_cx.append(cx)
                all_cy.append(cy)
                all_angles.append(eq)
                letter = 'W' if (eq > np.pi / 2 and eq < 3 * np.pi / 2) else 'M'
                all_letters.append(letter)
                all_dists.append(dist)
                all_snap_angles.append(eq)

    all_cx = np.array(all_cx)
    all_cy = np.array(all_cy)
    all_angles = np.array(all_angles)
    all_dists = np.array(all_dists)
    letters = np.array(all_letters)

    print(f'Found {len(all_cx)} equilibrium points')

    # --- FIGURE ---
    fig, axes = plt.subplots(1, 3, figsize=(18, 6), facecolor='#14111a')

    # 1. Classic bifurcation: distance vs. angle (vertical sweep through center)
    mask_center = np.abs(all_cx - WHEEL_CX) < (CANVAS * 0.05)
    colors_center = np.where(letters[mask_center] == 'M', '#9b6b9e', '#6b9e8c')
    axes[0].scatter(all_dists[mask_center], all_angles[mask_center],
                    c=colors_center, s=2, alpha=0.8)
    axes[0].set_xlabel('Control distance from center', color='#c8c2d0')
    axes[0].set_ylabel('Stable angle (radians)', color='#c8c2d0')
    axes[0].set_title('Bifurcation (vertical sweep)', color='#ede8f2')
    axes[0].set_facecolor('#14111a')
    axes[0].tick_params(colors='#7a7488')
    axes[0].axhline(y=np.pi / 2, color='#3d3650', linestyle='--', alpha=0.5)
    axes[0].axhline(y=3 * np.pi / 2, color='#3d3650', linestyle='--', alpha=0.5)

    # 2. Bistability map: where are there two stable states?
    # Count equilibria per grid cell
    bistable_x, bistable_y = [], []
    mono_m_x, mono_m_y = [], []
    mono_w_x, mono_w_y = [], []
    for cx in cx_range:
        for cy in cy_range:
            mask = (all_cx == cx) & (all_cy == cy)
            n_eq = np.sum(mask)
            if n_eq >= 2:
                bistable_x.append(cx)
                bistable_y.append(cy)
            elif n_eq == 1:
                if letters[mask][0] == 'M':
                    mono_m_x.append(cx)
                    mono_m_y.append(cy)
                else:
                    mono_w_x.append(cx)
                    mono_w_y.append(cy)

    axes[1].scatter(mono_m_x, mono_m_y, c='#9b6b9e', s=4, alpha=0.6, label='M only')
    axes[1].scatter(mono_w_x, mono_w_y, c='#6b9e8c', s=4, alpha=0.6, label='W only')
    axes[1].scatter(bistable_x, bistable_y, c='#e8a84c', s=6, alpha=0.8, label='Bistable')
    axes[1].plot(WHEEL_CX, WHEEL_CY, 'wo', markersize=8, markeredgecolor='#7a7488')
    axes[1].plot(FIXED_X, FIXED_Y, '^', color='#d4634a', markersize=8)
    axes[1].set_xlabel('Control x', color='#c8c2d0')
    axes[1].set_ylabel('Control y', color='#c8c2d0')
    axes[1].set_title('Bistability map (gold = catastrophe zone)', color='#ede8f2')
    axes[1].set_facecolor('#14111a')
    axes[1].set_aspect('equal')
    axes[1].tick_params(colors='#7a7488')
    axes[1].legend(fontsize=8, facecolor='#1c1826', edgecolor='#3d3650',
                   labelcolor='#c8c2d0')

    # 3. Horizontal sweep bifurcation
    mask_horiz = np.abs(all_cy - WHEEL_CY) < (CANVAS * 0.05)
    colors_horiz = np.where(letters[mask_horiz] == 'M', '#9b6b9e', '#6b9e8c')
    axes[2].scatter(all_cx[mask_horiz], all_angles[mask_horiz],
                    c=colors_horiz, s=2, alpha=0.8)
    axes[2].set_xlabel('Control x position', color='#c8c2d0')
    axes[2].set_ylabel('Stable angle (radians)', color='#c8c2d0')
    axes[2].set_title('Bifurcation (horizontal sweep)', color='#ede8f2')
    axes[2].set_facecolor('#14111a')
    axes[2].tick_params(colors='#7a7488')
    axes[2].axhline(y=np.pi / 2, color='#3d3650', linestyle='--', alpha=0.5)
    axes[2].axhline(y=3 * np.pi / 2, color='#3d3650', linestyle='--', alpha=0.5)

    for ax in axes:
        for spine in ax.spines.values():
            spine.set_color('#3d3650')

    plt.tight_layout()
    plt.savefig('bifurcation_diagram.png', dpi=200, facecolor='#14111a',
                bbox_inches='tight')
    print('Saved bifurcation_diagram.png')


if __name__ == '__main__':
    main()
