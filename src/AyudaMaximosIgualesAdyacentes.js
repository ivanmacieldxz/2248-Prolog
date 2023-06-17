import React from 'react';

function AyudaMaximosIgualesAdyacentes({ value, onClick }) {
    return (
        <div
            className='booster'
            onClick={onClick}
        >
            {value}
        </div>
    )
}

export default AyudaMaximosIgualesAdyacentes;